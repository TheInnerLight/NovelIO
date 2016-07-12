namespace NovelFS.NovelIO.UnitTests

open NovelFS.NovelIO

module EndianHelper =
    let convertToEndianness endianness arr =
        match endianness, System.BitConverter.IsLittleEndian with
        |(LittleEndian, false) | (BigEndian, true) -> Array.rev arr
        |_ -> arr