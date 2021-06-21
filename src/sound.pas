unit sound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, portaudio, sndfile, mpg123, log, ctypes;

type
  { TSound }

  TSound = class(TObject)
  private
    function GetChannels: integer; virtual; abstract;
    function GetSampleRate: integer; virtual; abstract;
    function GetAudioLength: double; virtual; abstract;
    function GetSampleFormat: PaSampleFormat; virtual; abstract;
    function GetSource: string; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    property Channels: integer read GetChannels;
    property SampleRate: integer read GetSampleRate;
    property Duration: double read GetAudioLength;
    property SampleFormat: PaSampleFormat read GetSampleFormat;
    property Source: string read GetSource;
    // Seek to begin
    procedure SeekToBeginning; virtual; abstract;
    // Read to buffer
    function Read(output: pointer; frameCount: longint): boolean; virtual; abstract;
  end;



  {The record to hold sound data }
  TSoundData = record
    Buffer: PAnsiChar;
    Size: integer;
    //Loaded: boolean;
  end;
  PSoundData = ^TSoundData;

  { The record passed to sndfile virtual file io callbacks }
  TSeekableSoundData = record
    Sound: PSoundData;
    Position: sf_count_t;
  end;
  PSeekableSoundData = ^TSeekableSoundData;

  {TSndSound}
  {Implementation of SndFile}
  TSndSound = class(TSound)
  private
    FSoundFile: PSNDFILE;

    FFileName: string;
    FAudioLength: double;

    FInfo: SF_INFO;

    FVirtualIOCallbacks: SF_VIRTUAL_IO;
    FVIOUserData: TSeekableSoundData;

    function GetAudioLength: double; override;
    function GetChannels: integer; override;
    function GetSampleFormat: PaSampleFormat; override;
    function GetSampleRate: integer; override;
    procedure SetFileName(AValue: string);
    function GetSource: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SeekToBeginning; override;
    function Read(output: pointer; frameCount: longint): boolean; override;

    property FileName: string read FFileName write SetFileName;
    procedure LoadInMemorySound(SoundData: PSoundData);
    procedure LoadDefaultSound;
    procedure LoadTick;
    property Channels: integer read GetChannels;
    property SampleRate: integer read GetSampleRate;
    property Duration: double read GetAudioLength;
    property SampleFormat: PaSampleFormat read GetSampleFormat;
    property Source: string read GetSource;
  end;

  {TMpgSound}
  {Implementation of Mpg123 for MP3}
  TMpgSound = class(TSound)
  private
    FHandle: Tmpg123_handle;
    FError: integer;

    FFileName: string;
    FAudioLength: double;

    FRate: cardinal;
    FChannelCount: integer;
    FEncoding: integer;

    function GetAudioLength: double; override;
    function GetChannels: integer; override;
    function GetSampleFormat: PaSampleFormat; override;
    function GetSampleRate: integer; override;
    procedure SetFileName(AValue: string);
    function GetSource: string; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure SeekToBeginning; override;
    function Read(output: pointer; frameCount: longint): boolean; override;

    property FileName: string read FFileName write SetFileName;
    property Channels: integer read GetChannels;
    property SampleRate: integer read GetSampleRate;
    property Duration: double read GetAudioLength;
    property SampleFormat: PaSampleFormat read GetSampleFormat;
    property Source: string read GetSource;
  end;

implementation

uses audio;

function sf_vio_get_filelen_impl(user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSeekableSoundData;
begin
  //Logger.Debug('Entering sf_vio_get_filelen_impl');
  SoundData := PSeekableSoundData(user_data);
  Result := SoundData^.Sound^.Size;
  //Logger.Debug('Return value is ' + IntToStr(SoundData^.Sound^.Size));
  //Logger.Debug('Exiting sf_vio_get_filelen_impl');
end;

function sf_vio_seek_impl(offset: sf_count_t; whence: cint;
  user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSeekableSoundData;
begin
  //Logger.Debug('Entering sf_vio_seek_impl');
  //Logger.Debug('');

  SoundData := PSeekableSoundData(user_data);

  //Logger.Debug('Offset - ' + IntToStr(offset));
  case whence of
    SEEK_CUR:
    begin
      //Logger.Debug('SEEK_CUR');
      SoundData^.Position := (SoundData^.Position) + offset;
    end;
    SEEK_SET:
    begin
      //Logger.Debug('SEEK_SET');
      SoundData^.Position := offset;
    end;
    SEEK_END:
    begin
      //Logger.Debug('SEEK_END');
      SoundData^.Position := (SoundData^.Sound^.Size) - 1 - offset;
    end;
  end;
  Result := SoundData^.Position;

  //Logger.Debug('');
  //Logger.Debug('Exiting sf_vio_seek_impl');
end;

function sf_vio_read_impl(ptr: pointer; Count: sf_count_t;
  user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSeekableSoundData;
  Position, ActualCount: sf_count_t;
begin
  //Logger.Debug('Entering sf_vio_read_impl');
  //Logger.Debug('');

  SoundData := PSeekableSoundData(user_data);

  //Logger.Debug('Count - ' + IntToStr(Count));
  //Logger.Debug('Size - ' + IntToStr(SoundData^.Sound^.Size));

  Position := SoundData^.Position;
  //Logger.Debug('Position - ' + IntToStr(SoundData^.Position));
  { Check if this fetch will go beyond the total size of the file buffer }
  if Position >= SoundData^.Sound^.Size then
    ActualCount := 0
  else if Position >= (SoundData^.Sound^.Size - Count) then
    ActualCount := (SoundData^.Sound^.Size) - Position
  else
    ActualCount := Count;

  Move(SoundData^.Sound^.Buffer[SoundData^.Position], ptr^, ActualCount);
  SoundData^.Position := (SoundData^.Position) + ActualCount;
  Result := ActualCount;

  //Logger.Debug('New position - ' + IntToStr(SoundData^.Position));

  //Logger.Debug('');
  //Logger.Debug('Exiting sf_vio_read_impl');
end;

function sf_vio_write_impl(const {%H-}ptr: pointer; {%H-}Count: sf_count_t;
  {%H-}user_data: pointer): sf_count_t; cdecl;
begin
  //Logger.Debug('Entering sf_vio_write_impl');
  Result := 0;
  //Logger.Debug('Exiting sf_vio_write_impl');
end;

function sf_vio_tell_impl(user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSeekableSoundData;
begin
  //Logger.Debug('Entering sf_vio_tell_impl');
  //Logger.Debug('');

  SoundData := PSeekableSoundData(user_data);
  Result := SoundData^.Position;

  //Logger.Debug('Position - ' + IntToStr(SoundData^.Position));
  //Logger.Debug('');
  //Logger.Debug('Exiting sf_vio_tell_impl');
end;

{ TSound }

constructor TSound.Create;
begin
  //inherited Create;
end;

destructor TSound.Destroy;
begin
  inherited Destroy;
end;

{ TMpgSound }

function TMpgSound.GetAudioLength: double;
begin
  Result := FAudioLength;
end;

function TMpgSound.GetChannels: integer;
begin
  Result := FChannelCount;
end;

function TMpgSound.GetSampleFormat: PaSampleFormat;
begin
  Result := paFloat32;
end;

function TMpgSound.GetSampleRate: integer;
begin
  Result := FRate;
end;

procedure TMpgSound.SetFileName(AValue: string);
var
  Dur: double;
  FrameLength: coff_t;
begin
  FError := mpg123_close(FHandle);
  if FError <> MPG123_OK then
  begin
    Logger.Debug('Error after mpg123_close ' + IntToStr(FError));
    //Exit;
  end;

  FError := mpg123_open(FHandle, PChar(AValue));
  if FError <> MPG123_OK then
  begin
    Logger.Debug('Error after mpg123_open ' + IntToStr(FError));
    //raise EInvalidAudio.Create('mpg123_open returned error for ' + AValue);
    FFileName := '';
    Exit;
  end;
  FFileName := AValue;
  Logger.Debug('MPEG audio file loaded: ' + FFileName);
  FError := mpg123_getformat(FHandle, FRate, FChannelCount, FEncoding);
  if FError <> MPG123_OK then
  begin
    //raise EInvalidAudio.Create('mpg123_getformat returned error for ' + AValue);

    FFileName := '';
    Exit;
  end;
  Logger.Debug('');
  Logger.Debug('Rate          - ' + IntToStr(FRate));
  Logger.Debug('Encoding      - ' + IntToStr(FEncoding));
  Logger.Debug('Channel count - ' + IntToStr(FChannelCount));

  FError := mpg123_scan(FHandle);
  if FError <> MPG123_OK then
  begin
    Logger.Debug('Error after mpg123_scan ' + IntToStr(FError));
  end;

  FrameLength := mpg123_length(FHandle);
  Logger.Debug('FrameLength is ' + IntToStr(FrameLength));
  if FrameLength < 0 then
  begin
    Logger.Debug('Error after mpg123_scan ' + IntToStr(FrameLength));
  end;

  {Dur :=  mpg123_tpf(FHandle);
  Logger.Debug('Duration is ' + FloatToStr(Duration));
  Logger.Debug('');

  if Dur < 0 then
  begin
    Logger.Debug('Error after mpg123_tpf ' + FloatToStr(Dur));
  end;}
  Dur := FrameLength / FRate;
  Logger.Debug('Duration is ' + FloatToStr(Dur));

  FAudioLength := Dur;
end;

function TMpgSound.GetSource: string;
begin
  Result := FFileName;
end;

constructor TMpgSound.Create();
var
  Rates: plong = nil;
  RateCount: size_t = 0;
  //X, Y: Pointer;
  Count: integer;
begin
  FHandle := nil;
  FHandle := mpg123_new(nil, FError);
  if FError <> MPG123_OK then
    Logger.Debug('Error after mpg123_new ' + IntToStr(FError));

  FError := mpg123_format_none(FHandle);
  if FError <> MPG123_OK then
    Logger.Debug('Error after mpg123_format_none ' + IntToStr(FError));

  //RateCount := 1;
  //X := Addr(Rates);
  //Y := Addr(RateCount);
  //Rates := nil;

  mpg123_rates(Rates, RateCount);

  if Rates <> nil then
    Logger.Debug('Rates is not nil');

  //Logger.Debug('After mpg123_rates. Rate count is ' + IntToStr(Rates[0]));


  for Count := 0 to RateCount - 1 do
  begin
    //Logger.Debug('Handling rate count ' + IntToStr(Rates[Count]));

    mpg123_format(FHandle, Rates[Count], MPG123_MONO or MPG123_STEREO,
      MPG123_ENC_FLOAT_32);
    if FError <> MPG123_OK then
      Logger.Debug('Error after mpg123_format ' + IntToStr(FError));

  end;

end;

destructor TMpgSound.Destroy;
begin
  FError := mpg123_close(FHandle);
  if FError <> MPG123_OK then
  begin
    Logger.Debug('Error after mpg123_close ' + IntToStr(FError));
  end;
  if FHandle <> nil then
    mpg123_delete(FHandle);
  inherited Destroy;
end;

procedure TMpgSound.SeekToBeginning;
begin
  FError := mpg123_seek(FHandle, 0, SEEK_SET);
end;

function TMpgSound.Read(output: pointer; frameCount: longint): boolean;
var
  done: size_t = 0;
begin
  FError := mpg123_read(FHandle, output, frameCount * SizeOf(cfloat) *
    FChannelCount, done);

  if done = 0 then
  begin
    Result := False;
    Exit;
  end;
  if FError <> MPG123_OK then
  begin
    //Logger.Debug('mpg123_read failed?');
    Result := False;
    Exit;
  end;
  Result := True;
end;

{ TSndSound }

procedure TSndSound.SetFileName(AValue: string);
var
  TempSoundFile: PSNDFILE;
begin
  //EnterCriticalSection(AudioCriticalSection);
  //if not AudioSystem.Loaded then
  //raise EAudioNotLoaded.Create('Audio not loaded.');

  {if FAudioPlaying then
  begin
    Logger.Debug('Cannot call SetFileName when audio is playing');
    //LeaveCriticalSection(AudioCriticalSection);
    Exit;
  end;}
  {if AValue = '' then
  begin
    FFileName := '';
    FAudioLength := 0;
    if FSoundFile <> nil then
      sf_close(FSoundFile);
    Exit;
  end;}


  TempSoundFile := sf_open(PChar(AValue), SFM_READ, @FInfo);
  if (TempSoundFile = nil) then
  begin
    Logger.Debug('Error in sf_open');
    //LeaveCriticalSection(AudioCriticalSection);
    //raise EInvalidAudio.Create('sf_open returned nil for ' + AValue);
    FFileName := '';
    Exit;
  end;

  if FSoundFile <> nil then
    sf_close(FSoundFile);

  FSoundFile := TempSoundFile;

  Logger.Debug('sf_open - format - ' + IntToHex(FInfo.format, 8));
  Logger.Debug('sf_open - channels - ' + IntToStr(FInfo.channels));
  Logger.Debug('sf_open - frames - ' + IntToStr(FInfo.frames));
  Logger.Debug('sf_open - samplerate - ' + IntToStr(FInfo.samplerate));
  Logger.Debug('sf_open - sections - ' + IntToStr(FInfo.sections));

  FFileName := AValue;
  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //LeaveCriticalSection(AudioCriticalSection);
end;

function TSndSound.GetSource: string;
begin
  Result := FFileName;
end;

function TSndSound.GetChannels: integer;
begin
  Result := FInfo.channels;
end;

function TSndSound.GetSampleFormat: PaSampleFormat;
begin
  Result := paFloat32;
end;

function TSndSound.GetAudioLength: double;
begin
  Result := FAudioLength;
end;

function TSndSound.GetSampleRate: integer;
begin
  Result := FInfo.samplerate;
end;

constructor TSndSound.Create;
begin
  //inherited create;
  FSoundFile := nil;
  with FVirtualIOCallbacks do
  begin
    get_filelen := @sf_vio_get_filelen_impl;
    Read := @sf_vio_read_impl;
    seek := @sf_vio_seek_impl;
    tell := @sf_vio_tell_impl;
    Write := @sf_vio_write_impl;
  end;

  FVIOUserData.Sound := @AudioSystem.DefaultSound;
  FVIOUserData.Position := 0;

end;

destructor TSndSound.Destroy;
begin
  if FSoundFile <> nil then
    sf_close(FSoundFile);
  inherited Destroy;
end;

procedure TSndSound.SeekToBeginning;
begin
  if sf_seek(FSoundFile, 0, SEEK_SET) = -1 then
  begin
    Logger.Error('Sf_seek returned error');
  end;
end;

function TSndSound.Read(output: pointer; frameCount: longint): boolean;
var
  readCount: cint;
  {Data: pcfloat;
  Count: integer;}
begin
  readCount := 0;
  readCount := sf_read_float(FSoundFile, output, frameCount * FInfo.channels);

  { Apply scaling to amplitude to control volume }
  {Data := output;
  for Count:=0 to readCount - 1 do
  begin
    Data[Count] := Data[Count] * 0.1;
  end;}

  // If read count matches what was requested, then all the stream has
  // not completed
  Result := (readCount > 0);
end;

procedure TSndSound.LoadInMemorySound(SoundData: PSoundData);
var
  TempSoundFile: PSNDFILE;
begin
  //EnterCriticalSection(AudioCriticalSection);
  //if not AudioSystem.Loaded then
  //raise EAudioNotLoaded.Create('Audio not loaded.');

  FVIOUserData.Sound := SoundData;
  FVIOUserData.Position := 0;

  Logger.Debug('Before calling sf_open_virtual');
  Logger.Debug('');
  Logger.Debug('Position - ' + IntToStr(FVIOUserData.Position));
  Logger.Debug('Size - ' + IntToStr(FVIOUserData.Sound^.Size));
  Logger.Debug('');

  TempSoundFile := sf_open_virtual(@FVirtualIOCallbacks, SFM_READ,
    @FInfo, @FVIOUserData);
  if (TempSoundFile = nil) then
  begin
    Logger.Debug('Error in sf_open_virtual');
    Exit;
  end;
  Logger.Debug('sf_open_virtual successful');
  if FSoundFile <> nil then
    sf_close(FSoundFile);

  FSoundFile := TempSoundFile;
  FFileName := '';

  Logger.Debug('sf_open_virtual - format - ' + IntToHex(FInfo.format, 8));
  Logger.Debug('sf_open_virtual - channels - ' + IntToStr(FInfo.channels));
  Logger.Debug('sf_open_virtual - frames - ' + IntToStr(FInfo.frames));
  Logger.Debug('sf_open_virtual - samplerate - ' + IntToStr(FInfo.samplerate));
  Logger.Debug('sf_open_virtual - sections - ' + IntToStr(FInfo.sections));

  Logger.Debug('');

  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //LeaveCriticalSection(AudioCriticalSection);

end;

procedure TSndSound.LoadDefaultSound;
begin
  Logger.Debug('Loading default sound');
  LoadInMemorySound(@AudioSystem.DefaultSound);
end;

procedure TSndSound.LoadTick;
begin
  Logger.Debug('Loading default tick');
  LoadInMemorySound(@AudioSystem.DefaultTick);
end;

end.
