use integer_encoding::VarInt;
use std::collections::VecDeque;
use std::io;
use std::io::Read;
use std::marker::PhantomData;
use std::mem::MaybeUninit;

const BUF_SIZE: usize = 8096;

pub struct BufferedRead<R: Read> {
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

impl<R: Read> BufferedRead<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            scratch: VecDeque::new(),
        }
    }

    pub fn fill_buff(&mut self, n: usize) -> io::Result<()> {
        while self.scratch.len() < n {
            fill!(self, return Err(io::Error::from(io::ErrorKind::UnexpectedEof)));
        }

        Ok(())
    }

    pub fn read_exact(&mut self, buf: &mut [MaybeUninit<u8>]) -> io::Result<()> {
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
            },
        }

        self.scratch.drain(..buf.len());
        Ok(())
    }

    pub fn next_chunk<const N: usize>(&mut self) -> io::Result<[u8; N]> {
        let mut buff = const { MaybeUninit::<[u8; N]>::uninit() };
        // Safety:
        // conversion between MaybeUninit<[u8; N]> -> [MaybeUninit<u8>; N]
        // is sound
        let buff_ref: &mut [MaybeUninit<u8>; N] = unsafe { &mut *buff.as_mut_ptr().cast() };
        self.read_exact(buff_ref)?;
        // buff has been filled if the call to self.read_exact succeeds
        Ok(unsafe { buff.assume_init() })
    }

    pub fn read(&mut self, n: usize) -> io::Result<Box<[u8]>> {
        let mut buff = Box::new_uninit_slice(n);
        self.read_exact(&mut buff)?;
        // buff has been filled if the call to self.read_exact succeeds
        Ok(unsafe { buff.assume_init() })
    }

    pub fn next_byte(&mut self) -> io::Result<u8> {
        self.fill_buff(1)?;
        Ok(self
            .scratch
            .pop_front()
            .expect("there has to be at least 1 byte within self.scratch"))
    }

    pub fn has_data(&mut self) -> io::Result<bool> {
        Ok(self.scratch.is_empty() && {
            fill!(self, return Ok(false));
            true
        })
    }
}



pub trait Int: VarInt {
    const MAX_SIZE: usize;
}

impl<VI: VarInt> Int for VI {
    const MAX_SIZE: usize = (size_of::<Self>() * 8 + 7) / 7;
}

const CONTINUE_BIT: u8 = 0b1000_0000;

struct Leb128Parser<T: Int> {
    buf: [u8; 10],
    i: usize,
    _marker: PhantomData<T>,
}

impl<T: Int> Leb128Parser<T> {
    pub fn new() -> Self {
        Leb128Parser {
            buf: [0; 10],
            i: 0,
            _marker: PhantomData,
        }
    }
    pub fn push(&mut self, b: u8) -> io::Result<()> {
        if self.i >= T::MAX_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Unterminated varint",
            ));
        }
        self.buf[self.i] = b;
        self.i += 1;
        Ok(())
    }
    pub fn finished(&self) -> bool {
        self.i > 0 && (self.buf[self.i - 1] & CONTINUE_BIT == 0)
    }
    pub fn decode<VI: VarInt>(&self) -> Option<VI> {
        Some(VI::decode_var(&self.buf[0..self.i])?.0)
    }
}

impl<R: Read> BufferedRead<R> {
    pub fn read_leb128<VI: Int>(&mut self) -> io::Result<VI> {
        let mut p = Leb128Parser::<VI>::new();
        while !p.finished() {
            p.push(self.next_byte()?)?;
        }
        p.decode()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "invalid varint read"))
    }
}
