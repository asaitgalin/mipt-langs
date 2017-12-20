-module(steganography).
-author("Andrey Saitgalin (andrey.saitgalin@gmail.com)").
-export([read_image/1, write_image/2, insert_text_to_image/2, extract_text_from_image/1]).

%% Header each BMP image begins with.
-define(BMP_HDR, "BM").

%% Right after common BMP header comes other header,
%% all new images have BITMAPV5HEADER and its size is below.
-define(BITMAPV5HEADER_SIZE, 124).

%% Only images with 24 bits per pixel and without compression are supported.
-define(BMP_SUPPORTED_BPP, 24).
-define(BMP_SUPPORTED_COMPRESSION, 0).

%% Each PNG image starts with this bytes.
-define(PNG_HDR, 16#89, $P, $N, $G, 16#0D, 16#0A, 16#1A, 16#0A).

%% Only images with 24 bits per pixel and compressed with zlib deflate are supported.
-define(PNG_SUPPORTED_COLOR_TYPE, 2#010).
-define(PNG_SUPPORTED_COMPRESSION, 0).
-define(PNG_SUPPORTED_BIT_DEPTH, 8).
-define(PNG_SUPPORTED_FILTERING, 0).

-record(image, {
    type,
    headers,
    contents
}).

read_image(bmp, Contents) ->
    case Contents of
        <<?BMP_HDR,_:32,Reserved1:16,Reserved2:16,Offset:32/little,StructSize:32/little,
          _Width:32/little-signed,_Height:32/little-signed,_:16,BitCount:16/little,
          Compression:32/little,_/binary>> ->
            case {Reserved1, Reserved2, StructSize} of
                {0, 0, ?BITMAPV5HEADER_SIZE} ->
                    case {BitCount, Compression} of
                        {?BMP_SUPPORTED_BPP, ?BMP_SUPPORTED_COMPRESSION} ->
                            Headers = binary_part(Contents, 0, Offset),
                            Pixels = binary_part(Contents, Offset, size(Contents) - Offset),
                            Image = #image{
                                type = bmp,
                                headers = Headers,
                                contents = Pixels
                            },
                            {ok, Image};
                        _ -> {error, "unsupported BMP image, only images with 24 bits "
                                     "per pixel without compression are supported"}
                    end;
                _ -> {error, "invalid BMP header"}
            end;
        _  -> {error, "file header is not a valid BMP image header"}
    end;
read_image(png, Contents) ->
    Zlib = zlib:open(),
    zlib:inflateInit(Zlib),
    <<?PNG_HDR, Chunks/binary>> = Contents,
    Res = process_chunks(Chunks, #image{}, Zlib),
    zlib:close(Zlib),
    Res;
read_image(_, _Contents) -> {error, "image file does not have any "
                             "supported image headers"}.

read_chunk(Contents) ->
    case Contents of
        <<Length:32/big, Type:4/binary, Rest/binary>> ->
            case Rest of
                <<ChunkData:Length/binary, CRC:32, Bytes/binary>> ->
                    Concatenated = binary_to_list(Type) ++ binary_to_list(ChunkData),
                    case erlang:crc32(list_to_binary(Concatenated)) == CRC of
                        true -> {binary_to_list(Type), ChunkData, Bytes};
                        false -> {error, "incorrect chunk checksum"}
                    end;
                _ -> {error, "invalid chunk header"}
            end;
        _ -> {error, "invalid chunk header"}
    end.

process_chunks(Contents, Image, Zlib) ->
    case read_chunk(Contents) of
        {Type, ChunkData, Rest} -> process_chunk(Type, ChunkData, Image, Rest, Zlib);
        Error -> Error
    end.

process_chunk("IHDR", ChunkData, _, Rest, Zlib) ->
    case ChunkData of
        <<_Width:32, _Height:32, BitDepth:8, ColorType:8, CompressionMethod:8,
          FilterMethod:8, _InterlaceMethod:8, _/binary >> ->
            case {BitDepth, ColorType, CompressionMethod, FilterMethod} of
                {?PNG_SUPPORTED_BIT_DEPTH, ?PNG_SUPPORTED_COLOR_TYPE,
                 ?PNG_SUPPORTED_COMPRESSION, ?PNG_SUPPORTED_FILTERING} ->
                    Img = #image{ headers = ChunkData , contents = [] },
                    process_chunks(Rest, Img, Zlib);
                _ -> {error, "unsupported image, only PNGs with 24bpp, "
                      "without filtering and zlib deflated are supported"}
            end;
        _ -> {error, "unsupported IHDR"}
    end;
process_chunk("IDAT", ChunkData, Image, Rest, Zlib) ->
    Uncompressed = zlib:inflate(Zlib, ChunkData),
    Bytes = binary_to_list(iolist_to_binary(Uncompressed)),
    Img = #image{
        headers = Image#image.headers,
        contents = lists:append(Image#image.contents, Bytes)
    },
    process_chunks(Rest, Img, Zlib);
process_chunk("IEND", _, Image, _, Zlib) ->
    zlib:inflateEnd(Zlib),
    <<Width:32, _/binary>> = Image#image.headers,
    Bytes = drop_filter_bytes(Image#image.contents, Width, []),
    Img = #image{
        type = png,
        contents = list_to_binary(Bytes),
        headers = Image#image.headers
    },
    {ok, Img};
%% Skipping unknown chunks.
process_chunk(_, _, Image, Rest, Zlib) ->
    process_chunks(Rest, Image, Zlib).

drop_filter_bytes([_ | Contents], Width, Acc) ->
    {Head, Tail} = lists:split(Width * 3, Contents),
    drop_filter_bytes(Tail, Width, lists:append(lists:reverse(Head), Acc));
drop_filter_bytes([], _, Acc) ->
    lists:reverse(Acc).

detect_image_type(Contents) ->
    case Contents of
        <<?BMP_HDR, _/binary>> -> bmp;
        <<?PNG_HDR, _/binary>> -> png;
        <<_/binary>> -> unknown
    end.

read_image(FileName) ->
    case file:read_file(FileName) of
        %% TODO(asaitgalin): No need to read the whole file to detect type.
        {ok, Contents} -> read_image(detect_image_type(Contents), Contents);
        {error, Reason} -> {error, Reason}
    end.

chunk(Type, Data) ->
    Length = byte_size(Data),
    TypeData = <<Type/binary, Data/binary>>,
    Crc = erlang:crc32(TypeData),
    binary_to_list(<<Length:32, TypeData/binary, Crc:32>>).

add_filter_byte(ChunkBytes, Width, Acc) when length(ChunkBytes) > 0 ->
    {Head, Tail} = lists:split(Width * 3, ChunkBytes),
    %% Only zero filtering is supported for now.
    add_filter_byte(Tail, Width, lists:append(lists:reverse([0 | Head]), Acc));
add_filter_byte([], _, Acc) ->
    lists:reverse(Acc).

%% Split image data to small blocks of 2 ^ 15 bytes, compress them and write each with IDAT header.
compress_chunks(ChunksBytes, Zlib, Acc) when length(ChunksBytes) > 0 ->
    {Head, Tail} = lists:split(min(1 bsl 15, length(ChunksBytes)), ChunksBytes),
    Compressed = iolist_to_binary(zlib:deflate(Zlib, list_to_binary(Head))),
    ChunkData = chunk(<<"IDAT">>, Compressed),
    compress_chunks(Tail, Zlib, lists:append(Acc, ChunkData));
compress_chunks([], Zlib, Acc) ->
    LastBytes = iolist_to_binary(zlib:deflate(Zlib, <<>>, finish)),
    ChunkData = chunk(<<"IDAT">>, LastBytes),
    lists:append(Acc, ChunkData).

write_image(Image, FileName) ->
    case Image#image.type of
        bmp ->
            HeaderBytes = binary_to_list(Image#image.headers),
            ContentBytes = binary_to_list(Image#image.contents),

            file:write_file(FileName, list_to_binary(lists:append(HeaderBytes, ContentBytes)));
        png ->
            Zlib = zlib:open(),
            zlib:deflateInit(Zlib),
            %% TODO(asaitgalin): Store it in separate field in record?
            <<Width:32, _/binary>> = Image#image.headers,
            Chunks = compress_chunks(
                add_filter_byte(binary_to_list(Image#image.contents), Width, []),
                Zlib,
                []),
            zlib:deflateEnd(Zlib),

            PNGContents = binary_to_list(<<?PNG_HDR>>) ++
                chunk(<<"IHDR">>, Image#image.headers) ++
                Chunks ++
                chunk(<<"IEND">>, <<>>),

            file:write_file(FileName, list_to_binary(PNGContents))
    end.

encode([], [], Acc) -> lists:reverse(Acc);
encode([_TextByte | _TextBytes], [], Acc) -> lists:reverse(Acc);
encode([], [ImageByte | ImageBytes], Acc) ->
    encode([(ImageByte band 2#00000011)], [ImageByte | ImageBytes], Acc);
encode([TextByte | TextBytes], [ImageByte | ImageBytes], Acc) ->
      Value = (ImageByte band 2#11111100) bor TextByte,
      encode(TextBytes, ImageBytes, [Value | Acc]).

encode_byte(Byte) ->
    Part1 = (Byte band 2#11000000) bsr 6,
    Part2 = (Byte band 2#00110000) bsr 4,
    Part3 = (Byte band 2#00001100) bsr 2,
    Part4 = Byte band 2#00000011,
    [Part1, Part2, Part3, Part4].

decode_byte([Part1, Part2, Part3, Part4]) ->
    Mask = 2#00000011,
    ((Part1 band Mask) bsl 6) bor
    ((Part2 band Mask) bsl 4) bor
    ((Part3 band Mask) bsl 2) bor
    Part4 band Mask.

split_text([], Acc) -> Acc;
split_text([Byte | Bytes], Acc) ->
    split_text(Bytes, lists:append(Acc, encode_byte(Byte))).

insert_text_to_image(Image, Text) ->
    ImageBytes = binary_to_list(Image#image.contents),
    TextBytes = binary_to_list(Text),
    TextLength = length(TextBytes),
    TextLengthBinary = <<TextLength:16/little>>,
    TextBytesWithLength = split_text(binary_to_list(TextLengthBinary) ++ TextBytes, []),
    #image{
        type = Image#image.type,
        headers = Image#image.headers,
        contents = list_to_binary(encode(TextBytesWithLength, ImageBytes, []))
    }.

extract_bytes(_, 0, Acc) ->
    {ok, lists:reverse(Acc)};
extract_bytes(List, Length, _) when length(List) < Length * 4 ->
    {error, "invalid image data"};
extract_bytes([Part1, Part2, Part3, Part4 | Rest], Length, Acc) ->
    extract_bytes(Rest, Length - 1, [decode_byte([Part1, Part2, Part3, Part4]) | Acc]).

extract_text_from_image(Image) ->
    ImageBytes = binary_to_list(Image#image.contents),
    Byte1 = decode_byte(lists:sublist(ImageBytes, 4)),
    Byte2 = decode_byte(lists:sublist(ImageBytes, 5, 4)),
    <<Length:16/little>> = <<Byte1, Byte2>>,
    extract_bytes(lists:sublist(ImageBytes, 9, length(ImageBytes) - 8), Length, []).
