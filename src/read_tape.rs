use std::collections::VecDeque;
use std::io;
use std::io::Read;
use std::mem::MaybeUninit;

const BUF_SIZE: usize = 8096;

pub struct ReadTape<R: Read> {
    reader: R,
    scratch: VecDeque<u8>,
}

macro_rules! fill {
    ($self: ident, $($eof_handler: tt)*) => {'fill: {
        let mut buff = [0; BUF_SIZE];
        let amt = $self.reader.read(&mut buff)?;
        if amt == 0 {
            $($eof_handler)*;

            #[allow(unreachable_code)] { break 'fill }
        }
        $self.scratch.extend(&buff[..amt])
    }};
}

impl<R: Read> ReadTape<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            scratch: VecDeque::new(),
        }
    }

    fn fill_buff(&mut self, n: usize) -> io::Result<()> {
        while self.scratch.len() < n {
            fill!(
                self,
                return Err(io::Error::from(io::ErrorKind::UnexpectedEof))
            );
        }

        Ok(())
    }

    fn read_exact(&mut self, buf: &mut [MaybeUninit<u8>]) -> io::Result<()> {
        self.fill_buff(buf.len())?;

        let copy_into = |from: &[u8], to: &mut [MaybeUninit<u8>]| {
            // Safety: u8 has the same layout as MaybeUninit<u8>
            let from = unsafe { &*(from as *const [u8] as *const [MaybeUninit<u8>]) };
            to.copy_from_slice(from)
        };

        let (front, back) = self.scratch.as_slices();
        match buf.split_at_mut_checked(front.len()) {
            None => copy_into(&front[..buf.len()], buf),
            Some((buf_front, buf_back)) => {
                let (back, _) = back.split_at(buf_back.len());
                copy_into(front, buf_front);
                copy_into(back, buf_back);
            }
        }

        self.scratch.drain(..buf.len());
        Ok(())
    }

    pub fn read_chunk<const N: usize>(&mut self) -> io::Result<[u8; N]> {
        let mut buff = const { MaybeUninit::<[u8; N]>::uninit() };
        // Safety:
        // conversion between MaybeUninit<[u8; N]> -> [MaybeUninit<u8>; N]
        // is sound
        let buff_ref: &mut [MaybeUninit<u8>; N] = unsafe { &mut *buff.as_mut_ptr().cast() };
        self.read_exact(buff_ref)?;
        // buff has been filled if the call to self.read_exact succeeds
        Ok(unsafe { buff.assume_init() })
    }

    pub fn read_byte(&mut self) -> io::Result<u8> {
        self.fill_buff(1)?;
        Ok(self
            .scratch
            .pop_front()
            .expect("there has to be at least 1 byte within self.scratch"))
    }

    pub fn peek_byte(&mut self) -> io::Result<u8> {
        self.fill_buff(1)?;
        Ok(self
            .scratch
            .front()
            .copied()
            .expect("there has to be at least 1 byte within self.scratch"))
    }

    pub fn read(&mut self, n: usize) -> io::Result<Box<[u8]>> {
        let mut buff = Box::new_uninit_slice(n);
        self.read_exact(&mut buff)?;
        // buff has been filled if the call to self.read_exact succeeds
        Ok(unsafe { buff.assume_init() })
    }

    pub fn has_data(&mut self) -> io::Result<bool> {
        Ok(!self.scratch.is_empty() && {
            fill!(self, return Ok(false));
            true
        })
    }
}

mod sealed {
    use std::io;
    use std::io::Read;
    use crate::read_tape::ReadTape;

    pub trait Sealed: Sized {
        fn read_leb128<R: Read>(reader: &mut ReadTape<R>) -> io::Result<Self>;
    }
}

pub trait Int: sealed::Sealed {}

impl<T: sealed::Sealed> Int for T {}

const CONTINUE_BIT: u8 = 0b10000000;
const SIGN_BIT: u8     = 0b010000000;
const DATA_BITS: u8 = !CONTINUE_BIT;

macro_rules! impl_int {
    ($($t: ty)*) => {$(
        impl sealed::Sealed for $t {
            fn read_leb128<R: Read>(reader: &mut ReadTape<R>) -> io::Result<Self> {
                let mut result = 0 as $t;
                let mut shift = 0;
                let mut byte;
                loop {
                    byte = reader.read_byte()?;
                    // as per the spec one shall continue to read data even if there is runoff
                    result |= ((byte & DATA_BITS) as $t) << shift;
                    shift += 7;
                    if byte & CONTINUE_BIT == 0 {
                        break
                    }
                }
                
                #[allow(unused_comparisons)]
                const IS_SIGNED: bool = const { <$t>::MIN < 0 };
                
                if IS_SIGNED && shift < <$t>::BITS && (byte & SIGN_BIT != 0) {
                    /* sign extend */
                    result |= ((!0) << shift);
                }
                
                Ok(result)
            }
        }
    )*};
}

impl_int! {
    i8 i16 i32 i64 i128
    u8 u16 u32 u64 u128
}

impl<R: Read> ReadTape<R> {
    pub fn read_leb128<I: Int>(&mut self) -> io::Result<I> {
        I::read_leb128(self)
    }
}
