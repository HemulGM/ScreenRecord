unit AviWriter;

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
//       AviWriter -- a component to create rudimentary AVI files          //
//                  by Elliott Shevin, with large pieces of code           //
//                  stolen from Anders Melander                            //
//       version 1.0. Please send comments, suggestions, and advice        //
//       to shevine@aol.com.                                               //
/////////////////////////////////////////////////////////////////////////////

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                      Video for Windows                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Adapted from Thomas Schimming's VFW.PAS                                    //
// (c) 1996 Thomas Schimming, schimmin@iee1.et.tu-dresden.de                  //
// (c) 1998,99 Anders Melander                                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Ripped all COM/ActiveX stuff and added some AVI stream functions.          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
// Unicode version created by Arnold and TLama (2012)                         //
////////////////////////////////////////////////////////////////////////////////
// Part adding by HemulGM
////////////////////////////////////////////////////////////////////////////////

type
  LONG = Longint;

  PVOID = Pointer;

const
  // TAVIFileInfo dwFlag values
  AVIF_HASINDEX = $00000010;
  AVIF_MUSTUSEINDEX = $00000020;
  AVIF_ISINTERLEAVED = $00000100;
  AVIF_WASCAPTUREFILE = $00010000;
  AVIF_COPYRIGHTED = $00020000;
  AVIF_KNOWN_FLAGS = $00030130;
  AVIERR_UNSUPPORTED = $80044065; // MAKE_AVIERR(101)
  AVIERR_BADFORMAT = $80044066; // MAKE_AVIERR(102)
  AVIERR_MEMORY = $80044067; // MAKE_AVIERR(103)
  AVIERR_INTERNAL = $80044068; // MAKE_AVIERR(104)
  AVIERR_BADFLAGS = $80044069; // MAKE_AVIERR(105)
  AVIERR_BADPARAM = $8004406A; // MAKE_AVIERR(106)
  AVIERR_BADSIZE = $8004406B; // MAKE_AVIERR(107)
  AVIERR_BADHANDLE = $8004406C; // MAKE_AVIERR(108)
  AVIERR_FILEREAD = $8004406D; // MAKE_AVIERR(109)
  AVIERR_FILEWRITE = $8004406E; // MAKE_AVIERR(110)
  AVIERR_FILEOPEN = $8004406F; // MAKE_AVIERR(111)
  AVIERR_COMPRESSOR = $80044070; // MAKE_AVIERR(112)
  AVIERR_NOCOMPRESSOR = $80044071; // MAKE_AVIERR(113)
  AVIERR_READONLY = $80044072; // MAKE_AVIERR(114)
  AVIERR_NODATA = $80044073; // MAKE_AVIERR(115)
  AVIERR_BUFFERTOOSMALL = $80044074; // MAKE_AVIERR(116)
  AVIERR_CANTCOMPRESS = $80044075; // MAKE_AVIERR(117)
  AVIERR_USERABORT = $800440C6; // MAKE_AVIERR(198)
  AVIERR_ERROR = $800440C7; // MAKE_AVIERR(199)

  // TAVIStreamInfo dwFlag values
  AVISF_DISABLED = $00000001;
  AVISF_VIDEO_PALCHANGES = $00010000;
  AVISF_KNOWN_FLAGS = $00010001;

type
  TAVIFileInfoW = record
    dwMaxBytesPerSec, // max. transfer rate
    dwFlags,         // the ever-present flags
    dwCaps, dwStreams, dwSuggestedBufferSize, dwWidth, dwHeight, dwScale, dwRate, // dwRate / dwScale == samples/second
    dwLength, dwEditCount: DWORD;
    szFileType: array[0..63] of WideChar; // descriptive string for file type?
  end;

  PAVIFileInfoW = ^TAVIFileInfoW;

  TAVIStreamInfoW = record
    fccType, fccHandler, dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority, wLanguage: WORD;
    dwScale, dwRate, // dwRate / dwScale == samples/second
    dwStart, dwLength, // In units above...
    dwInitialFrames, dwSuggestedBufferSize, dwQuality, dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount, dwFormatChangeCount: DWORD;
    szName: array[0..63] of WideChar;
  end;

  TAVIStreamInfo = TAVIStreamInfoW;

  PAVIStreamInfo = ^TAVIStreamInfo;

  PAVIStream = pointer;

  PAVIFile = pointer;

  TAVIStreamList = array[0..0] of PAVIStream;

  PAVIStreamList = ^TAVIStreamList;

  TAVISaveCallback = function(nPercent: integer): LONG; stdcall;

  TAVICompressOptions = packed record
    fccType: DWORD;
    fccHandler: DWORD;
    dwKeyFrameEvery: DWORD;
    dwQuality: DWORD;
    dwBytesPerSecond: DWORD;
    dwFlags: DWORD;
    lpFormat: Pointer;
    cbFormat: DWORD;
    lpParms: Pointer;
    cbParms: DWORD;
    dwInterleaveEvery: DWORD;
  end;

  PAVICompressOptions = ^TAVICompressOptions;

// Palette change data record
const
  RIFF_PaletteChange: DWORD = 1668293411;

type
  TAVIPalChange = packed record
    bFirstEntry: byte;
    bNumEntries: byte;
    wFlags: WORD;
    peNew: array[byte] of TPaletteEntry;
  end;

  PAVIPalChange = ^TAVIPalChange;

  APAVISTREAM = array[0..1] of PAVISTREAM;

  APAVICompressOptions = array[0..1] of PAVICompressOptions;

procedure AVIFileInit; stdcall;

procedure AVIFileExit; stdcall;

function AVIFileOpen(var ppfile: PAVIFile; szFile: PChar; uMode: UINT; lpHandler: pointer): HResult; stdcall;

function AVIFileCreateStream(pfile: PAVIFile; var ppavi: PAVISTREAM; var psi: TAVIStreamInfo): HResult; stdcall;

function AVIStreamSetFormat(pavi: PAVIStream; lPos: LONG; lpFormat: pointer; cbFormat: LONG): HResult; stdcall;

function AVIStreamReadFormat(pavi: PAVIStream; lPos: LONG; lpFormat: pointer; var cbFormat: LONG): HResult; stdcall;

function AVIStreamWrite(pavi: PAVIStream; lStart, lSamples: LONG; lpBuffer: pointer; cbBuffer: LONG; dwFlags: DWORD; var plSampWritten: LONG; var plBytesWritten: LONG): HResult; stdcall;

function AVIStreamRelease(pavi: PAVISTREAM): ULONG; stdcall;

function AVIFileRelease(pfile: PAVIFile): ULONG; stdcall;

function AVIFileGetStream(pfile: PAVIFile; var ppavi: PAVISTREAM; fccType: DWORD; lParam: LONG): HResult; stdcall;

function CreateEditableStream(var ppsEditable: PAVISTREAM; psSource: PAVISTREAM): HResult; stdcall;

function AVISaveV(szFile: PChar; pclsidHandler: PGUID; lpfnCallback: TAVISaveCallback; nStreams: integer; pavi: APAVISTREAM; lpOptions: APAVICompressOptions): HResult; stdcall;

const
  AVIERR_OK = 0;
  AVIIF_LIST = $01;
  AVIIF_TWOCC = $02;
  AVIIF_KEYFRAME = $10;
  StreamtypeVIDEO = $73646976; // DWORD( 'v', 'i', 'd', 's' )
  StreamtypeAUDIO = $73647561; // DWORD( 'a', 'u', 'd', 's' )

type
  TAviWriter = class(TComponent)
  private
    FTempFileName: string;
    FAVIFile: PAVIFile;
    FHeight: integer;
    FWidth: integer;
    FStretch: Boolean;
    FFrameTime: integer;
    FFileName: string;
    FVideoStream: PAVISTREAM;
    FAudioStream: PAVISTREAM;
    FNeedSetInfo: Boolean;
    FFrameIndex: Integer;
    FAVIStream: PAVISTREAM;
    FPixelFormat: TPixelFormat;
    procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer; var ImageSize: longInt; PixelFormat: TPixelFormat);
    function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
    procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader; PixelFormat: TPixelFormat);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Save;
    procedure AddVideo(Stream: TStream);
    procedure AddAudio(const WavFileName: string);
  public
    property Height: integer read FHeight write FHeight;
    property Width: integer read FWidth write FWidth;
    property FrameTime: integer read FFrameTime write FFrameTime;
    property Stretch: boolean read FStretch write FStretch;
    property FileName: string read FFileName write FFileName;
    property FrameIndex: Integer read FFrameIndex;
    property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat;
  end;

implementation

procedure AVIFileInit; stdcall; external 'avifil32.dll' name 'AVIFileInit';

procedure AVIFileExit; stdcall; external 'avifil32.dll' name 'AVIFileExit';

function AVIFileOpen; external 'avifil32.dll' name 'AVIFileOpenW';

function AVIFileCreateStream; external 'avifil32.dll' name 'AVIFileCreateStreamW';

function AVIStreamSetFormat; external 'avifil32.dll' name 'AVIStreamSetFormat';

function AVIStreamReadFormat; external 'avifil32.dll' name 'AVIStreamReadFormat';

function AVIStreamWrite; external 'avifil32.dll' name 'AVIStreamWrite';

function AVIStreamRelease; external 'avifil32.dll' name 'AVIStreamRelease';

function AVIFileRelease; external 'avifil32.dll' name 'AVIFileRelease';

function AVIFileGetStream; external 'avifil32.dll' name 'AVIFileGetStream';

function CreateEditableStream; external 'avifil32.dll' name 'CreateEditableStream';

function AVISaveV; external 'avifil32.dll' name 'AVISaveVW';

constructor TAviWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrameTime := 1000;
  FPixelFormat := pf24bit;
  FStretch := False;
  FFileName := '';
  AVIFileInit;
  FTempFileName := {tempdir +} 'temp.avi';
end;

destructor TAviWriter.Destroy;
begin
  AviFileExit;
  inherited;
end;

procedure TAviWriter.AddVideo(Stream: TStream);
var
  BitmapInfo: PBitmapInfoHeader;
  BitmapInfoSize: Integer;
  BitmapSize: longInt;
  BitmapBits: pointer;
  StretchBitmap: TBitmap;
  Bitmap: TBitmap;
  Samples_Written: LONG;
  Bytes_Written: LONG;
  AVIERR: integer;
begin
  Bitmap := TBitmap.Create;
  try
    BitmapInfo := nil;
    BitmapBits := nil;
    try
      Bitmap.LoadFromStream(Stream);
      if FStretch and ((Bitmap.Width <> FWidth) or (Bitmap.Height <> FHeight)) then
      begin
        StretchBitmap := TBitmap.Create;
        try
          StretchBitmap.SetSize(FWidth, FHeight);
          StretchBitmap.Canvas.StretchDraw(Rect(0, 0, FWidth, FHeight), Bitmap);
        finally
          Bitmap.Free;
        end;
        Bitmap := StretchBitmap;
      end;
      // Determine size of DIB
      InternalGetDIBSizes(Bitmap.Handle, BitmapInfoSize, BitmapSize, FPixelFormat);
      if (BitmapInfoSize = 0) then
        raise Exception.Create('Failed to retrieve bitmap info');

      // Get DIB header and pixel buffers
      GetMem(BitmapInfo, BitmapInfoSize);
      GetMem(BitmapBits, BitmapSize);
      InternalGetDIB(Bitmap.Handle, 0, BitmapInfo^, BitmapBits^, FPixelFormat);

      // On the first time through, set the stream format.
      if FNeedSetInfo then
      begin
        if (AVIStreamSetFormat(FAVIStream, 0, BitmapInfo, BitmapInfoSize) <> AVIERR_OK) then
          raise Exception.Create('Failed to set AVI stream format');
        FNeedSetInfo := False;
      end;

      // Write frame to the video stream
      AVIERR := AVIStreamWrite(FAVIStream, FFrameIndex, 1, BitmapBits, BitmapSize, AVIIF_KEYFRAME, Samples_Written, Bytes_Written);
      if AVIERR <> AVIERR_OK then
        raise Exception.Create('Failed to add frame to AVI. Err=' + IntToHex(AVIERR, 8))
      else
        Inc(FFrameIndex);
    finally
      if (BitmapInfo <> nil) then
        FreeMem(BitmapInfo);
      if (BitmapBits <> nil) then
        FreeMem(BitmapBits);
    end;
  finally
    Bitmap.free;
  end;
end;

procedure TAviWriter.AddAudio(const WavFileName: string);
var
  InputFile: PAVIFILE;
  InputStream: PAVIStream;
  ErrorText: string;
  State: Int64;
begin
  // Open the audio file.
  State := AVIFileOpen(InputFile, pchar(WavFileName), OF_READ, nil);
  if State = AVIERR_BADFORMAT then
    ErrorText := 'The file could not be read, indicating a corrupt file or an unrecognized format.';
  if State = AVIERR_MEMORY then
    ErrorText := 'The file could not be opened because of insufficient memory.';
  if State = AVIERR_FILEREAD then
    ErrorText := 'A disk error occurred while reading the file.';
  if State = AVIERR_FILEOPEN then
    ErrorText := 'A disk error occurred while opening the file.';
  if State = REGDB_E_CLASSNOTREG then
    ErrorText := 'According to the registry, the type of file specified in AVIFileOpen does not have a handler to process it.';
  if ErrorText <> '' then
    raise Exception.Create(ErrorText);

  // Open the audio stream.
  try
    if AVIFileGetStream(InputFile, InputStream, 0, 0) <> AVIERR_OK then
      raise Exception.Create('Unable to get audio stream');
    try
      if CreateEditableStream(FAudioStream, InputStream) <> AVIERR_OK then
        raise Exception.Create('Failed to create editable AVI audio stream');
    finally
      AviStreamRelease(InputStream);
    end;
  finally
    AviFileRelease(InputFile);
  end;
end;

// Converts a bitmap to a DIB of a specified PixelFormat.
function TAviWriter.InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := (GetDIBits(DC, Bitmap, 0, Abs(TBitmapInfoHeader(BitmapInfo).biHeight),
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0);
  finally
    if OldPal <> 0 then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

// Calculates the buffer sizes nescessary for convertion of a bitmap to a DIB of a specified PixelFormat.
procedure TAviWriter.InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer; var ImageSize: longInt; PixelFormat: TPixelFormat);
var
  Info: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, Info, PixelFormat);
  if Info.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (Info.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl Info.biBitCount);
  ImageSize := Info.biSizeImage;
end;

// Fills a TBitmapInfoHeader with the values of a bitmap when converted to a DIB of a specified PixelFormat.
procedure TAviWriter.InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader; PixelFormat: TPixelFormat);
var
  DIB: TDIBSection;
  Bytes: Integer;

  function AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal;
  begin
    Dec(Alignment);
    Result := ((Bits * BitsPerPixel) + Alignment) and not Alignment;
    Result := Result shr 3;
  end;

begin
  DIB.dsBmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DIB), @DIB);
  if Bytes = 0 then
    raise Exception.Create('Invalid bitmap');

  if (Bytes >= (SizeOf(DIB.dsBm) + SizeOf(DIB.dsBmih))) and (DIB.dsBmih.biSize >= SizeOf(DIB.dsBmih)) then
    Info := DIB.dsBmih
  else
  begin
    FillChar(Info, SizeOf(Info), 0);
    with Info, DIB.dsBm do
    begin
      biSize := SizeOf(Info);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case PixelFormat of
    pf1bit:
      Info.biBitCount := 1;
    pf4bit:
      Info.biBitCount := 4;
    pf8bit:
      Info.biBitCount := 8;
    pf24bit:
      Info.biBitCount := 24;
    pf32bit:
      Info.biBitCount := 32;
  else
    raise Exception.Create('Invalid pixel format');
  end;
  Info.biPlanes := 1;
  Info.biCompression := BI_RGB;
  Info.biSizeImage := AlignBit(Info.biWidth, Info.biBitCount, 32) * Cardinal(Abs(Info.biHeight));
end;

procedure TAviWriter.Save;
var
  Streams: APAVISTREAM;
  CompOptions: APAVICompressOptions;
  AVIERR: integer;
  StreamCount: integer;
begin
  try
    if CreateEditableStream(FVideoStream, FAVIStream) <> AVIERR_OK then
      raise Exception.Create('Could not create Video Stream');
  finally
    AviStreamRelease(FAVIStream);
  end;

  if FAudioStream <> nil then
    StreamCount := 2
  else
    StreamCount := 1;

  Streams[0] := FVideoStream;
  Streams[1] := FAudioStream;
  CompOptions[0] := nil;
  CompOptions[1] := nil;

  AVIERR := AVISaveV(PChar(FileName), nil, nil, StreamCount, Streams, CompOptions);
  if AVIERR <> AVIERR_OK then
    raise Exception.Create('Unable to write output file');

  if Assigned(FVideoStream) then
    AviStreamRelease(FVideoStream);
  if Assigned(FAudioStream) then
    AviStreamRelease(FAudioStream);

  try
    while AviFileRelease(FAVIFile) > 0 do { loop }
  except
    // ignore exception
  end;

  DeleteFile(FTempFileName);
  FNeedSetInfo := True;
end;

procedure TAviWriter.Start;
var
  State: Int64;
  ErrorText: string;
  StreamInfo: TAVIStreamInfo;
begin
  FAudioStream := nil;
  FVideoStream := nil;
  FNeedSetInfo := True;
  FFrameIndex := 0;

  FAVIFile := nil;
  State := AVIFileOpen(FAVIFile, PChar(FTempFileName), OF_CREATE or OF_WRITE or OF_SHARE_EXCLUSIVE, nil);
  if State = AVIERR_BADFORMAT then
    ErrorText := 'The file could not be read, indicating a corrupt file or an unrecognized format.';
  if State = AVIERR_MEMORY then
    ErrorText := 'The file could not be opened because of insufficient memory.';
  if State = AVIERR_FILEREAD then
    ErrorText := 'A disk error occurred while reading the file.';
  if State = AVIERR_FILEOPEN then
    ErrorText := 'A disk error occurred while opening the file.';
  if State = REGDB_E_CLASSNOTREG then
    ErrorText := 'According to the registry, the type of file specified in AVIFileOpen does not have a handler to process it.';
  if ErrorText <> '' then
    raise Exception.Create(ErrorText);

  FillChar(StreamInfo, SizeOf(StreamInfo), 0);
  // Set frame rate and scale
  StreamInfo.dwRate := 1000;
  StreamInfo.dwScale := FFrameTime;
  StreamInfo.fccType := StreamtypeVIDEO;
  StreamInfo.fccHandler := 0;
  StreamInfo.dwFlags := 0;
  StreamInfo.dwSuggestedBufferSize := 0;
  StreamInfo.rcFrame.Right := self.width;
  StreamInfo.rcFrame.Bottom := self.height;

  // Open AVI data stream
  if (AVIFileCreateStream(FAVIFile, FAVIStream, StreamInfo) <> AVIERR_OK) then
    raise Exception.Create('Failed to create AVI video stream');
end;

end.

