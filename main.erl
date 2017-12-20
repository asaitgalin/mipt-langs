#!/usr/bin/env escript

-module(steganography_script).
-author("Andrey Saitgalin (andrey.saitgalin@gmail.com)").

-import(steganography, [
    read_image/1,
    write_image/2,
    insert_text_to_image/2,
    extract_text_from_image/1
]).

-export([main/1]).

main([ImageFile, OutputImageFile, Text]) ->
    FilteredText = list_to_binary(lists:filter(fun(X) -> X < 256 end, Text)),
    case read_image(ImageFile) of
        {ok, Image} -> write_image(insert_text_to_image(Image, FilteredText), OutputImageFile);
        {error, Reason} -> io:fwrite("Failed to read image; ~s~n", [Reason])
    end;
main([ImageFile]) ->
    case read_image(ImageFile) of
        {ok, Image} ->
            case extract_text_from_image(Image) of
                {ok, Text} -> io:fwrite("~s~n", [Text]);
                {error, Reason} -> io:fwrite("Failed to extract text from image; ~s~n", [Reason])
            end;
        {error, Reason} -> io:fwrite("Failed to read image; ~s~n", [Reason])
    end;
main(_) ->
    io:fwrite("Usage:~n~n"
              "Insert text to image:~n"
              "./main.erl image_file output_image_file text_string~n~n"
              "Extract text from image:~n"
              "./main.erl image_file~n"),
    halt(1).
