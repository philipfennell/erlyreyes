-module(tiff).
-export([imageFileHeader/1, imageFileDirectory/8, metadata/1, metadata/4]).
-compile(export_all).


%% Image Field Directory macro used in sortFields/1.
-define(Field, {{tag,TagName}, {type,Type}, {count,Count}, {value,Value}}).

%% A record that represents the top-level structure of the TIFF image.
-record(image,{header, ifd, data}).




%% Writes a default image to the file system as terms.
dump() -> 
	dumpImage(makeImage(rgb, 4, 4, 72, inch, 
			makeTestImage(4, 4, {33, 66, 99})), "dump.txt").




%% Writes a default image to the file system as a binary.
%% @spec makeImage() -> atom()
makeImage() -> 
	serializeImage(big, makeImage(rgb, 16, 16, 72, inch, makeTestImage(16, 16, {255, 128, 64})), "default.tiff").




%% Returns the unserialized image.
%% @spec makeImage(Format::atom(), Width::integer(), Height::integer(), Res::integer(), ResUnit::atom(), ImageData::list()) -> tuple()
makeImage(Format, Width, Height, Res, ResUnit, ImageData) -> 
	Header = imageFileHeader(erlang:length(ImageData) * 3),
	#image{
		header=Header,
		ifd=imageFileDirectory(Format, Width, Height, Res, ResUnit, {bps, 8}, tiles, 8),
		data=ImageData
	}.




%% Returns the image file header
%% @spec imageFileHeader(IFD_Offset::integer()) -> list()
imageFileHeader(IFD_Offset) -> [{byte_order,big}, {identifier,42}, {offset,IFD_Offset + 8}].




%% Returns an Image File Directory.
%% @spec imageFileDirectory(Format::atom(), Width::integer(), Height::integer(), Res:integer(), ResUnit::atom(), atom(), DataOrgMethod::atom(), ImageDataOffset::integer) -> list().
imageFileDirectory(Format, Width, Height, Res, ResUnit, {bps, BPS}, DataOrgMethod, ImageDataOffset) ->
	SamplesPerPixel = samples4Format(Format),
	
	% Construct a list of the main fields.
	BaseLine = [
		newSubfileType(full),
		imageWidth(Width),
		imageLength(Height),
		bitsPerSample([BPS,BPS,BPS]),
		compression(uncompressed),
		photometricInterpretation(Format),
		planarConfiguration(chunky),
		samplesPerPixel(samples4Format(Format)),
		xResolution(Res),
		yResolution(Res),
		resolutionUnit(ResUnit)
	],
	
	% Construct a list of the data organization fields (strips or tiles).
	DataOrg = dataOrganisation(DataOrgMethod, Width, Height, SamplesPerPixel, ImageDataOffset),
	
	% Construct a list of metadata fields for the image.
	Metadata = metadata("Test Image."),
	
	% Add the lists together.
	IFD = BaseLine ++ DataOrg ++ Metadata,
	
	[{field_count,erlang:length(IFD)}|IFD].




%% Returns default metadata for the image.
metadata(ImageDescription) ->
	metadata("Philip A. R. Fennell", "2008", erlang:localtime(), ImageDescription).




%% Returns metadata for passed fields.
metadata(Artist, Copyright, DateTime, ImageDescription) ->
	[
		artist(Artist),
		copyright(Copyright),
		dateTime(DateTime),
		imageDescription(ImageDescription),
		software("E-Reyes v0.1")
	].




%% Returns a list of the fields that define the organisation of the image data.
dataOrganisation(tiles, ImageWidth, ImageLength, SamplesPerPixel, ImageDataOffset) ->
	TileWidth = 16,
	TileLength = 16,
	TilesAcross = erlang:trunc((ImageWidth + TileWidth - 1) / TileWidth),
	TilesDown = erlang:trunc((ImageLength + TileLength - 1) / TileLength),
	TilesPerImage = TilesAcross * TilesDown,
	TileByteCount = (TileWidth * TileLength) * SamplesPerPixel,
	[
		tileWidth(TileWidth),
		tileLength(TileLength),
		tileOffsets(TilesPerImage, TileByteCount, ImageDataOffset),
		tileByteCounts(TilesPerImage, TileByteCount)
	];
	
dataOrganisation(strips, ImageWidth, ImageLength, SamplesPerPixel, ImageDataOffset) -> 
	[
		stripOffsets([5,5,5,5]),
		rowsPerStrip(4),
		stripByteCounts([5,5,5,5])
	].




%% Serializes the passed image data.
dumpImage(#image{header=Header, ifd=[_FieldCount|IFD], data=_ImageData}, FileName) -> 
	% Image Header.
	write("header.txt", Header),
	
	
	% Image File Directory.
	write("ifd.txt", IFD).
	
	
	% Image Data.
	




%% Serializes the passed image data and writes as a binary stream to the file 
%% system.
%% @spec serializeImage(Endian::atom(), ???, FileName::string()) -> atom()
serializeImage(Endian, #image{header=Header, ifd=IFD, data=ImageData}, FileName) -> 

	% Image Data.
	SerializedImageData = serializeImageData(Endian, ImageData),
	
	% Image Header.
	SerialisedHeader = serializeHeader(Endian, Header),
	IFD_Offset = erlang:length(SerialisedHeader) + erlang:length(SerializedImageData),
	
	% io:format("IFD Offset = ~w~n", [IFD_Offset]),
	
	% Image File Directory.
	SerialisedIFD = serializeIFD(IFD_Offset, Endian, IFD),
	
	% io:format("Image Data Offset = ~w~n", [erlang:length(SerialisedIFD)]),
	
	% io:format("Image Pixel Count = ~w~n", [erlang:length(ImageData)]),
	
	% lists:foreach(fun(X) -> io:format("~w~n", [X]) end, ImageData),
	
	% Output the image to the file system.
	write(FileName, SerialisedHeader ++ SerializedImageData ++ SerialisedIFD).






%% Utility functions.
%% =============================================================================


%% Writes Data to a file called FileName on the filesystem.
write(FileName, Data) ->
	
	case file:open(FileName ,write) of 
		{ok, IoDevice} ->
			lists:foreach(fun(Y) -> io:fwrite(IoDevice, "~c", [Y]) end, Data),
			file:close(IoDevice);
		{error, Why} ->
			{error, Why}
	end.




%% Returns the passed IFD as a list of binary value lists.
fieldsToList([]) -> [];

fieldsToList([H|T]) ->
	[erlang:binary_to_list(H) | fieldsToList(T)].




%% Serializes the header to a binary sequence.
%% @spec serializeHeader(Endia::atom(), list()) -> list()
serializeHeader(Endian, [{byte_order,ByteOrder}, {identifier,Id}, {offset,Offset}]) -> 
	BOCharCode = case ByteOrder of
		big -> 77;
		little -> 49
	end,
	
	Binary = case Endian of 
		big ->		<<BOCharCode:8/big,		BOCharCode:8/big,	
				Id:16/big,		Offset:32/big>>;
				
		little ->	<<BOCharCode:8/little,	BOCharCode:8/little, 
				Id:16/little,	Offset:32/little>>
	end,
	
	erlang:binary_to_list(Binary).




%% Serializes the Image File Directory.
%% @spec serializeIFD(Offset::integer(), Endian::atom(), list()) -> list()
serializeIFD(Offset, Endian, [{field_count,FieldCount}|IFD]) ->
	
	io:format("~n~nFields~n"),
	lists:foreach(fun(X) -> io:format("~w~n", [X]) end, IFD),
	
	SortedFields = sortFields(lists:map(fun(X) -> fieldAtomsToNumbers(X) end, IFD)),
	% For some reason, that I haven't worked out yet, we need a fiddle-factor of 6.
	Fields = serializeFields2(Endian, Offset + (12 * FieldCount) + 6, {fields, SortedFields}),
	IFDOffsetValues = lists:flatten(lists:map(fun(X) -> serializeOffsetValue(Endian, X) end, SortedFields)),
	
	% io:format("Field Count = ~w~n", [FieldCount]),
	
	% io:format("~n~nFields Values~n"),
	% lists:foreach(fun(X) -> io:format("~c", [X]) end, IFDOffsetValues),
	
	% Field Count.
	SerialisedFields = lists:flatten(Fields),
	SerialisedFieldCount = case Endian of 
		big ->		<<FieldCount:16/big>>;
		little ->	<<FieldCount:16/little>>
	end,
	
	% Offset to next IFD, which in this case there is none, therefore the value
	% zero is used as <<0,0,0,0>>.
	Serialized_NextIFD_Offset = erlang:binary_to_list(<<0:32/big>>),
	
	erlang:binary_to_list(SerialisedFieldCount) ++ SerialisedFields ++ 
			Serialized_NextIFD_Offset ++ IFDOffsetValues.




%% Serializes the value from a directory field according to the values length
%% and ensure the value is word aligned.
%% @spec serializeOffsetValue(Endian::atom(), tuple()) -> list()
serializeOffsetValue(Endian, {Tag, {type,Type}, _, {value,Value}}) -> 
	SerializedValue = lists:flatten(serializeValue(Endian, Type, atomToValue(Value))),
	
	% Add a trailing null byte to ensure the following value is word aligned.
	case erlang:length(SerializedValue) rem 2 of 
		1 ->	SerializedValue ++ null();
		0 ->	SerializedValue
	end.




%% @spec serializeFieldValue(Endian::atom(), Type::atom(), Value::any(), Justify::tuple()) -> list()
serializeFieldValue(Endian, Type, Value, Justify) -> 
	ShiftBy = 4 - typeByteCount(Type),
	SerializedValue = serializeValue(Endian, Type, Value),
	
	case Justify of 
		{justify,left}	-> leftJustify(SerializedValue, Type, ShiftBy);
		{justify,right}	-> SerializedValue
	end.




%% Returns a list of octets that represent the value of the field.
%% @spec serializeValue(Endian::atom(), Type::atom(), list()) -> list()
serializeValue(Endian, Type, []) -> [];

serializeValue(Endian, Type, [H|T]) -> 
	Bits = typeByteCount(Type) * 8,
	Value = case Endian of 
		big ->		<<H:Bits/big>>;
		little ->	<<H:Bits/little>>
	end,
	[erlang:binary_to_list(Value) | serializeValue(Endian, Type, T)];

%% @spec serializeValue(Endian::atom(), Type::atom(), Value::any()) -> 
serializeValue(Endian, rational, Value) ->
	Bits = typeByteCount(rational) * 8,
	SerializedValue = erlang:binary_to_list(
		case Endian of 
			big ->		<<Value:32/big,		1:32/big>>;
			little ->	<<Value:32/little,	1:32/little>>
		end
	);

%% @spec serializeValue(Endian::atom(), Type::atom(), Value::any()) -> 
serializeValue(Endian, Type, Value) ->
	Bits = typeByteCount(Type) * 8,
	% ShiftBy = 4 - typeByteCount(Type),
	SerializedValue = erlang:binary_to_list(
		case Endian of 
			big ->		<<Value:Bits/big>>;
			little ->	<<Value:Bits/little>>
		end
	).




%% Returns the past value left-justified within its four byte field and 
%% according to its on length (byte count).
%% @spec leftJustifyValue(Value::any(), ::atom(), ShiftBy::integer()) -> any()
leftJustify(Value, byte, ShiftBy) when ShiftBy == 3 -> Value ++ [0,0,0];

leftJustify(Value, _, ShiftBy) when ShiftBy == 2 -> Value ++ [0,0];

leftJustify(Value, _, _) -> Value.




%% Serializes a field as a binary sequence.
%% @spec serializeField(Endian::atom(), tuple()) -> tuple()
serializeField(Offset, Endian, {{tag,Tag}, {type,TypeAtom}, {count,Count}, {value,Value}}) -> 
	Type = atomToTypeValue(TypeAtom),
	TypeByteCount = typeByteCount(TypeAtom),
	Length = TypeByteCount * Count,
	
	OffsetValue = case Length =< 4 of 
		true	-> atomToValue(Value);
		false	-> Offset + (Length rem 2)
	end,
	
	% io:format("ValueOffset: ~w ~w~n", [Tag, OffsetValue]),
	
	Field = case Endian of 
		big ->		<<Tag:16/big,		Type:16/big,		Count:32/big,		OffsetValue:32/big>>;
		little ->	<<Tag:16/little,	Type:16/little,		Count:32/little,	OffsetValue:32/little>>
	end,
	
	{{byte_count, Length}, {field, Field}}.




%% Serializes a list of IFDs to a list of binary sequences.
%% @spec serializeFields2(ValueOffset::integer(), list()) -> list()
serializeFields2(_, ValueOffset, {fields, []}) -> [];

%% @spec serializeFields2(ValueOffset::integer(), list()) -> list()
serializeFields2(Endian, ValueOffset, {fields, [FH|FT]}) -> 
	% io:format("ValueOffset = ~w~n", [ValueOffset]),
	
	% Serialize the value.
	{{tag,Tag}, {type,Type}, {count,Count}, {value,Value}} = FH,
	SerializedValue = lists:flatten(serializeFieldValue(Endian, Type, atomToValue(Value), {justify,right})),
	
	% 
	ValueLength = erlang:length(SerializedValue),
	OffsetValue = case ValueLength rem 2 of 
		1 ->	SerializedValue ++ null();
		0 ->	SerializedValue
	end,
	
	OffsetValueLength = erlang:length(OffsetValue),
	
	% Decide by value length if it is an offset or a value you are storing in 
	% the field.
	FieldValue = case ValueLength =< 4 of 
		true	-> serializeFieldValue(Endian, Type, atomToValue(Value), {justify,left});
		false	-> serializeFieldValue(Endian, long, ValueOffset, {justify,right})
	end,
	
	% io:format("Tag: ~w, Length: ~w, FieldValue: ~w~n", [Tag, ValueLength, FieldValue]),
	
	{field, SerializedField} = serializeField2(Endian, {{tag,Tag}, {type,Type}, {count,Count}, {value,FieldValue}}),
	
	[SerializedField | serializeFields2(Endian, ValueOffset + OffsetValueLength, {fields,FT})].




%% Serializes a field as a binary sequence.
%% @spec serializeField(Endian::atom(), tuple()) -> tuple()
serializeField2(Endian, {{tag,Tag}, {type,TypeAtom}, {count,Count}, {value,FieldValue}}) -> 
	Type = atomToTypeValue(TypeAtom),
	
	Field = case Endian of 
		big ->		<<Tag:16/big,		Type:16/big,		Count:32/big>>;
		little ->	<<Tag:16/little,	Type:16/little,		Count:32/little>>
	end,
	
	{field, erlang:binary_to_list(Field) ++ FieldValue}.




%% Serializes the image data (RGB pixel values) and flattens the resulting
%% list of lists.
%% @spec serializeImageData(Endian::atom(), ImageData::list()) -> list()
serializeImageData(Endian, ImageData) ->
	lists:flatten(serializePixels(Endian, ImageData)).




%% Serializes the pixel colour values.
%% @spec serializePixels(Endian::atom(), ImageData::list()) -> list()
serializePixels(_Endian, []) -> [];

serializePixels(Endian, [{R,G,B}|T]) -> 
	PixelValue = case Endian of 
		big ->		<<R:8/big,		G:8/big,	B:8/big>>;
		little ->	<<R:8/little,	G:8/little,	B:8/little>>
	end,
	
	[binary_to_list(PixelValue) | serializeImageData(Endian, T)].




%% Returns the IFD entry with the field tags and Types converted from a atoms to 
%% values.
%% @spec fieldAtomsToNumbers(Field::tuple()) -> tuple()
fieldAtomsToNumbers({{tag,TagAtom}, Type, Count, Value}) ->
	{{tag,atomToTagName(TagAtom)}, Type, Count, Value}.




%% Returns the integer value for the tag name atom.
%% @spec atomToTagName(Atom::atom()) -> integer()
atomToTagName(Atom) -> 
	case Atom of 
		new_subfile_type			-> 254;
		image_width					-> 256;
		image_length				-> 257;
		bits_per_sample				-> 258;
		compression					-> 259;
		photometric_interpretation	-> 262;
		image_description			-> 270;
		strip_offsets				-> 273;
		samples_per_pixel			-> 277;
		rows_per_strip				-> 278;
		strip_byte_counts			-> 279;
		x_resolution 				-> 282;
		y_resolution 				-> 283;
		planar_configuration		-> 284;
		resolution_unit				-> 296;
		software					-> 305;
		date_time					-> 306;
		artist						-> 315;
		tile_width					-> 322;
		tile_length					-> 323;
		tile_offsets				-> 324;
		tile_byte_counts			-> 325;
		copyright					-> 33432
	end.




%% Returns the integer value for the data type atom.
%% @spec atomToTypeValue(Atom::atom()) -> integer()
atomToTypeValue(Atom) -> 

	case Atom of      
		byte		->	1;	% 8-bit unsigned integer.
		ascii 		->	2;	% 8-bit byte that contains a 7-bit ASCII code; the 
							% last byte must be NUL (binary zero).
		short 		->	3;	% 16-bit (2-byte) unsigned integer.
		long 		->	4;	% 32-bit (4-byte) unsigned integer.
		rational 	->	5;	% Two LONGs: the first represents the numerator of 
							% a fraction; the second, the denominator.
		sbyte 		->	6;	% An 8-bit signed (twos-complement) integer.
		undefined 	->	7;	% An 8-bit byte that may contain anything, 
							% depending on the definition of the field.
		sshort 		->	8;	% A 16-bit (2-byte) signed (twos-complement) integer.
		slong 		->	9;	% A 32-bit (4-byte) signed (twos-complement) integer.
		srational 	->	10; % Two SLONG’s: the first represents the numerator 
							% of a fraction, the second the denominator.
		single 		->	11; % Single precision (4-byte) IEEE format 
							% (float is a reserved keyword in Erlang).
		double 		->	12 	% Double precision (8-byte) IEEE format.
	end.




%% Returns the integer value for the data type atom.
%% @spec atomToTypeValue(Atom::atom()) -> integer()
typeByteCount(Atom) -> 

	case Atom of      
		byte		->	1;	% 8-bit unsigned integer.
		ascii 		->	1;	% 8-bit byte that contains a 7-bit ASCII code; the 
							% last byte must be NUL (binary zero).
		short 		->	2;	% 16-bit (2-byte) unsigned integer.
		long 		->	4;	% 32-bit (4-byte) unsigned integer.
		rational 	->	8;	% Two LONGs: the first represents the numerator of 
							% a fraction; the second, the denominator.
		sbyte 		->	1;	% An 8-bit signed (twos-complement) integer.
		undefined 	->	1;	% An 8-bit byte that may contain anything, 
							% depending on the definition of the field.
		sshort 		->	2;	% A 16-bit (2-byte) signed (twos-complement) integer.
		slong 		->	4;	% A 32-bit (4-byte) signed (twos-complement) integer.
		srational 	->	8;	% Two SLONG’s: the first represents the numerator 
							% of a fraction, the second the denominator.
		single 		->	4;	% Single precision (4-byte) IEEE format 
							% (float is a reserved keyword in Erlang).
		double 		->	8 	% Double precision (8-byte) IEEE format.
	end.




%% Translates atoms to values or leave a value alone.
%% @spec atomToValue(Value::any()) -> any()
%% Unit of Resolution.
atomToValue(big)				-> "MM";
atomToValue(little)				-> "II";

%% Unit of Resolution.
atomToValue(none)				-> 1;
atomToValue(inch)				-> 2;
atomToValue(centimeter)			-> 3;

%% Planar Configuration.
atomToValue(chunky)				-> 1;
atomToValue(planar)				-> 2;

%% Compression.
atomToValue(uncompressed)		-> 1;
atomToValue(ccit)				-> 2;
atomToValue(group3fax)			-> 3;
atomToValue(group4fax)			-> 4;
atomToValue(lzw)				-> 5;
atomToValue(jpeg)				-> 6;
atomToValue(packbits)			-> 32773;

%% Photometric Interpretation.
atomToValue(white_is_zero)		-> 0;
atomToValue(black_is_zero)		-> 1;
atomToValue(rgb)				-> 2;
atomToValue(palette_color)		-> 3;
atomToValue(transparency_mask)	-> 4;
atomToValue(cmyk)				-> 5;
atomToValue(ycbcr)				-> 6;
atomToValue(cielab)				-> 8;

%%
atomToValue(full)				-> 0;
atomToValue(reduced)			-> 1;
atomToValue(page)				-> 2;
atomToValue(mask)				-> 4;

%% Fallback for non-atom values.
atomToValue([])					-> null();
atomToValue([H|T])				-> [H|T];
atomToValue(Value) 				-> Value.




%% Returns the IFD with its fields in numerical order.
%% @spec sortFields(list()) -> list()
sortFields([]) -> [];

sortFields([?Field|T]) -> 
	lists:append(lists:append(sortFields([X || X <- T, X < ?Field]),
			[?Field]), 
					sortFields([X || X <- T, X >= ?Field])).




%% Returns a list of RGB pixel values for the passed width/length of image.
makeTestImage(Width, Length, ColourValues) ->
	makeTestImageData(Width * Length, ColourValues, []).




%% Returns a list of pixel values. The values could be a tuple, or a list,
%% or whatever takes your fancy!
makeTestImageData(0, _, ImageData) -> ImageData;

makeTestImageData(PixelCount, ColourValues, ImageData) -> 
	makeTestImageData(PixelCount - 1, ColourValues, [ColourValues|ImageData]).




%% Returns a data type atom (short or long) for the passed integer value.
isShortOrLong(Value) when Value =< 65535 -> short;
isShortOrLong(Value) when Value > 65535 -> long.




%% Returns the number of samples for the passed image format.
samples4Format(Format) ->
	case Format of 
		bilevel 		-> 1;
		grayscale 		-> 1;
		palette_color 	-> 1;
		rgb 			-> 3
	end.




%% Takes the passed string and pre-processes it by adding a Null byte (binary 0)
%% to the end.
%% @spec preProcessString(String::string()) -> tuple()
preProcessString(String) -> 
	String2 = String ++ null(),
	{ok, {string,String2}, {count,erlang:length(String2)}}.




%% Generates a null byte
%% @spec null() -> list()
null() ->
	erlang:binary_to_list(<<0:8/big>>).






%% Baseline Required TIFF Fields for Full Colour RGB Images.
%% =============================================================================


%% Returns the ImageWidth IFD (256).
imageWidth(IW) -> 
	{{tag,image_width}, {type,isShortOrLong(IW)}, {count,1}, {value,IW}}.




%% Returns the ImageLength IFD (257).
imageLength(IL) -> 
	{{tag,image_length}, {type,isShortOrLong(IL)}, {count,1}, {value,IL}}.




%% Returns the BitsPerSample IFD (258).
bitsPerSample(BPS) -> 
	{{tag,bits_per_sample}, {type,short}, {count,erlang:length(BPS)}, {value,BPS}}.




%% Returns the Compression IFD (259).
compression(Comp) -> 
	case Comp of 
		uncompressed	-> true;
		ccit			-> true;
		group3fax		-> true;
		group4fax		-> true;
		lzw				-> true;
		jpeg			-> true;
		packbits		-> true
	end,
	{{tag,compression}, {type,short}, {count,1}, {value,Comp}}.




%% Returns the PhotometricInterpretation IFD (262).
photometricInterpretation(PhotoInterp) -> 
	case PhotoInterp of 
		white_is_zero		-> true;
		black_is_zero		-> true;
		rgb					-> true;
		palette_color		-> true;
		transparency_mask	-> true;
		cmyk				-> true;
		ycbcr				-> true;
		cielab				-> true
	end,
	{{tag,photometric_interpretation}, {type,short}, {count,1}, {value,PhotoInterp}}.




%% Returns the Planar Configuration IFD (284).
planarConfiguration(PlanarConfig) -> 
	case PlanarConfig of 
		chunky	-> true;
		planar	-> true
	end,
	{{tag,planar_configuration}, {type,short}, {count,1}, {value,PlanarConfig}}.




%% Returns the StripOffsets IFD (273).
%% Use StripsPerImage for the count,
%% Although SHORT or LONG is allowed for the type, use LONG as default.
%% Value will be a pointer to the list of strip offsets.
stripOffsets(SO) -> 
	{{tag,strip_offsets}, {type,long}, {count,erlang:length(SO)}, {value,SO}}.




%% Returns the SamplesPerPixel IFD (277).
samplesPerPixel(SPP) -> 
	{{tag,samples_per_pixel}, {type,short}, {count,1}, {value,SPP}}.




%% Returns the RowsPerStrip IFD (278).
rowsPerStrip(RPS) -> 
	{{tag,rows_per_strip}, {type,long}, {count,1}, {value,RPS}}.




%% Returns the StripByteCounts IFD (279)
stripByteCounts(SBC) -> 
	{{tag,strip_byte_counts}, {type,isShortOrLong(SBC)}, {count,erlang:length(SBC)}, {value,SBC}}.




%% Returns the XResolution IFD (282).
xResolution(X) -> 
	{{tag,x_resolution}, {type,rational}, {count,1}, {value,X}}.




%% Returns the YResolution IFD (283).
yResolution(Y) -> 
	{{tag,y_resolution}, {type,rational}, {count,1}, {value,Y}}.




%% Returns the ResolutionUnit IFD (296)
resolutionUnit(ResUnit) ->
	case ResUnit of 
		none		-> true;
		inch		-> true;
		centimeter	-> true
	end,
	{{tag,resolution_unit}, {type,short}, {count,1}, {value,ResUnit}}.






%% Additional Baseline Fields.
%% =============================================================================

%% Returns the NewSubfile Type IFD (254).
%% Person who created the image.
newSubfileType(Type) ->
	case Type of
		full	-> true;
		reduced	-> true;
		page	-> true;
		mask	-> true
	end,
	{{tag,new_subfile_type}, {type,long}, {count,1}, {value,Type}}.


	

%% Returns the Artist IFD (315).
%% Person who created the image.
artist(Art) ->
	{_, {string,String}, {count,Count}} = preProcessString(Art),
	{{tag,artist}, {type,ascii}, {count,Count}, {value,String}}.




%% Returns the Copyright IFD (33432).
%% Copyright notice.
copyright(Copyright) ->
	{_, {string,String}, {count,Count}} = preProcessString(Copyright),
	{{tag,copyright}, {type,ascii}, {count,Count}, {value,String}}.




%% Returns the DateTime IFD (306) for the current local time.
%% Date and time of image creation.
%% @spec dateTime() -> tuple()
dateTime() -> 
	dateTime(erlang:localtime()).




%% Returns the DateTime IFD (306) for the passed date/time tuple.
%% Date and time of image creation in the format YYYY:MM:DD HH:MM:SS.
%% @spec dateTime(tuple()) -> tuple()
dateTime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	DateTime = lists:concat([Year, ":", normalizeDateTimeValue(Month), ":", 
					normalizeDateTimeValue(Day), " ", 
							normalizeDateTimeValue(Hour), ":", 
									normalizeDateTimeValue(Minute), ":", 
											normalizeDateTimeValue(Second)]),
	{_, {string,String}, {count,Count}} = preProcessString(DateTime),
	{{tag,date_time}, {type,ascii}, {count,Count}, {value,String}}.




%% Prepends a date/time value with a 0 if it is less than 10.
%% @spec normalizeDateTimeValue(integer()) -> integer()
normalizeDateTimeValue(Value) when Value < 10 -> 
	lists:concat(["0", Value]);
normalizeDateTimeValue(Value) when Value >= 10 -> 
	Value.




%% Returns the ImageDescription IFD (270).
%% A string that describes the subject of the image.
imageDescription(ImageDesc) -> 
	{_, {string,String}, {count,Count}} = preProcessString(ImageDesc),
	{{tag,image_description}, {type,ascii}, {count,Count}, {value,String}}.




%% Returns the Software IFD (305).
%% Name and version number of the software package(s) used to create the image.
software(Software) -> 
	{_, {string,String}, {count,Count}} = preProcessString(Software),
	{{tag,software}, {type,ascii}, {count,Count}, {value,String}}.






%% TIFF Extension Fields.
%% =============================================================================


%% Returns the TileWidth IFD (322).
tileWidth(TW) -> 
	{{tag,tile_width}, {type,isShortOrLong(TW)}, {count,1}, {value,TW}}.



%% Returns the TileLength IFD (323).
tileLength(TL) -> 
	{{tag,tile_length}, {type,isShortOrLong(TL)}, {count,1}, {value,TL}}.




%% Returns the TileOffsets IFD (324).
tileOffsets(TilesPerImage, TileByteCount, ImageDataOffset) -> 
	{{tag,tile_offsets}, {type,long}, {count,TilesPerImage}, {value,ImageDataOffset}}.




%% Returns the TileByteCounts IFD (325).
tileByteCounts(TilesPerImage, TileByteCount) -> 
	{{tag,tile_byte_counts}, {type,isShortOrLong(TileByteCount)}, {count,TilesPerImage}, {value,TileByteCount}}.

