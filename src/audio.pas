unit audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sndfile, mpg123, portaudio, EventLog, ctypes, Forms,
  Dialogs, LCLIntf, lcltype, fgl, Math;

const
  READ_NOTLOADED = -1;
  READ_SND = 0;

  SampleRate = 44100;
  FramesPerBuffer = 1024;

  // How long you want to play the test sine:
  NumSecs = 2;

  // Wavetable size. Influences your pitch:
  TableSize = 200;

  MAX_VOLUME = 100;
  DEFAULT_VOLUME = 90;
  VOLUME_LOG_BASE = 10;

//SAudioFile = '{6decd475-7e30-461a-989c-995bb233ad7a}';

type
  EAudioNotLoaded = class(Exception);
  EInvalidDevice = class(Exception);
  EInvalidAudio = class(Exception);
  AudioDeviceIndex = PaDeviceIndex;

  TAudioDevice = record
    HostAPIName: string;
    DeviceName: string;
  end;

  PAudioDevice = ^TAudioDevice;
  TAudioDeviceList = specialize TFPGList<PAudioDevice>;

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

  { The record passed to port audio callbacks }
  TUserInfo = record
    Sound: TSound;
    Player: Pointer;
    Looped: boolean;
    Volume: double;

  end;
  PAudioInfo = ^TUserInfo;

  {The record to hold sound data }
  TSoundData = record
    Buffer: PAnsiChar;
    Size: integer;
    Loaded: boolean;
  end;
  PSoundData = ^TSoundData;

  { The record passed to sndfile virtual file io callbacks }
  TSndVIOUserData = record
    Sound: PSoundData;
    Position: sf_count_t;
  end;
  PSndVIOUserData = ^TSndVIOUserData;

  { A type which holds a wavetable, two integers keeping track of
    at which offset in the wavetable each channel is currently
    playing (= phase), and a message: }
  PaTestData = record
    Sine: array[0..TableSize] of CFloat;
    LeftPhase: CInt32;
    RightPhase: CInt32;
    AMessage: PChar;
    Player: Pointer;
  end;
  PPaTestData = ^PaTestData;


  {TSndSound}
  {Implementation of SndFile}
  TSndSound = class(TSound)
  private
    FSoundFile: PSNDFILE;

    FFileName: string;
    FAudioLength: double;

    FInfo: SF_INFO;

    FVirtualIOCallbacks: SF_VIRTUAL_IO;
    FVIOUserData: TSndVIOUserData;

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

  { TAudio }
  TAudio = class(TObject)
  private

    FUserInfo: TUserInfo;
    FStream: PPaStream;

    FAudioPlaying: boolean;

    Data: PaTestData;
    DataPointer: PPaTestData;
    class procedure LoadSoundFromResource(ResourceName: string;
      var Sound: TSoundData); static;

    class function GetDevices: TAudioDeviceList; static;
    class procedure SetOutputDevice(AValue: TAudioDevice); static;
    //procedure SetVolume(AValue: integer);

  public
    Loaded: boolean; static;
    UseDefaultDevice: boolean; static;
    FDevices: TAudioDeviceList; static;
    FDefaultDevice: integer; static;
    //AudioCriticalSection: TRTLCriticalSection; static;
    FOutputDevice: TAudioDevice; static;
    //FVolume: integer;

    {Default sounds }
    DefaultSound: TSoundData; static;
    DefaultTick: TSoundData; static;

    OnPlayCompletion: TNotifyEvent;
    Looped: boolean;
    constructor Create;
    destructor Destroy; override;
    class function GetDefaultDeviceIndex: AudioDeviceIndex; static;
    class procedure GetDefaultDevice(Device: PAudioDevice); static;
    class function GetDeviceIndex(Device: TAudioDevice): AudioDeviceIndex; static;
    class property DefaultDeviceIndex: AudioDeviceIndex read GetDefaultDeviceIndex;
    class property Devices: TAudioDeviceList read GetDevices;
    //property Volume: integer read FVolume write SetVolume;
    class procedure CleanUp; static;
    class procedure SetDefaulDevice; static;
    class procedure LoadDefaultSounds; static;
    class procedure FreeDefaultSounds; static;
    procedure Play(Sound: TSound; PlayLooped: boolean = False;
      Volume: integer = DEFAULT_VOLUME);

    procedure Abort;

    procedure FinishedAud({%H-}Datax: PtrInt);

    class function LoadSound(Avalue: string): TSound; static;

    property Playing: boolean read FAudioPlaying;
    class property OutputDevice: TAudioDevice read FOutputDevice write SetOutputDevice;

  end;


implementation

uses
  settings;

{This function is called by PortAudio to request for audio data, and is passed
as a parameter while opening the stream.
It tries to read from the sound file and supply the data.
If read does not return any data, or if it is less than the number of frames
requested, it assumes tha the entire data has been exhausted and
it closes the sound file and returns paComplete.
This will stop the stream and the associated callback - which
triggers on stoppage of the steam - gets called.}

function FeedAudioStream({%H-}input: pointer; output: pointer; frameCount: culong;
  {%H-}timeInfo: PPaStreamCallbackTimeInfo; {%H-}statusFlags: PaStreamCallbackFlags;
  userData: pointer): cint; cdecl;
var
  AudioInfo: PAudioInfo;
  readSuccess: boolean;
  UsedSound: TSound;
  Data: pcfloat;
  Count: integer;
begin
  //EnterCriticalSection(TAudio.AudioCriticalSection);
  //Logger.Debug('Inside FeedAudioStream');

  AudioInfo := PAudioinfo(userData);

  Assert(AudioInfo^.Sound <> nil);

  UsedSound := AudioInfo^.Sound;

  readSuccess := False;
  readSuccess := UsedSound.Read(output, frameCount);

  { Apply scaling to amplitude to control volume }
  Data := output;
  for Count := 0 to (AudioInfo^.Sound.Channels * frameCount) - 1 do
  begin
    Data[Count] := Data[Count] * AudioInfo^.Volume;
  end;


  if AudioInfo^.Looped then
  begin
    { If audio is lopped and if no audio data could be read,
    seek to the beginning and then read again }
    if not readSuccess then
    begin

      UsedSound.SeekToBeginning;

      readSuccess := False;
      readSuccess := UsedSound.Read(output, frameCount);

      { Apply scaling to amplitude to control volume }
      Data := output;
      for Count := 0 to (AudioInfo^.Sound.Channels * frameCount) - 1 do
      begin
        Data[Count] := Data[Count] * AudioInfo^.Volume;
      end;

      if not readSuccess then
      begin
        Logger.Debug('readCount zero immediately after seek to beginning');
        Result := cint(paAbort);
        //LeaveCriticalSection(TAudio.AudioCriticalSection);
        Exit;
      end;
    end;
    { If audio is looped, always continue }
    Result := cint(paContinue);
  end { when audio is not looped }
  else
  begin
    if readSuccess then
    begin
      Result := cint(paContinue);
    end
    else
    begin
      Result := cint(paComplete);
    end;
  end;

  //LeaveCriticalSection(TAudio.AudioCriticalSection);
end;

{This function is called by PortAudio to signal that the stream has stopped.
As this is a non-class function, it sends a message to frame using the frame's
handle.}
procedure AudioStreamFinished(UserData: pointer); cdecl;
var
  AudioInfo: PAudioInfo;
  {%H-}AudioTemp: TAudio; // Possibly buggy hint, omitting
begin
  //EnterCriticalSection(TAudio.AudioCriticalSection);

  AudioInfo := PAudioinfo(userData);

  AudioTemp := TAudio(AudioInfo^.Player);

  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  //LeaveCriticalSection(TAudio.AudioCriticalSection);
end;

{ This is called when playback is finished.
  Remember: ALWAYS USE CDECL or your pointers will be messed up!
  Pointers to this function must be castable to PPaStreamFinishedCallback: }
{procedure StreamFinished(UserData: pointer); cdecl;
var
  LocalDataPointer: PPaTestData;
  {%H-}AudioTemp: TAudio; // buggy hint, omitting
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);
  LocalDataPointer := PPaTestData(UserData);
  AudioTemp := TAudio(LocalDataPointer^.Player);
  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  LeaveCriticalSection(TAudio.AudioCriticalSection);
end;}

function sf_vio_get_filelen_impl(user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
begin
  Logger.Debug('Entering sf_vio_get_filelen_impl');
  SoundData := PSndVIOUserData(user_data);
  Result := SoundData^.Sound^.Size;
  Logger.Debug('Return value is ' + IntToStr(SoundData^.Sound^.Size));
  Logger.Debug('Exiting sf_vio_get_filelen_impl');
end;

function sf_vio_seek_impl(offset: sf_count_t; whence: cint;
  user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
begin
  Logger.Debug('Entering sf_vio_seek_impl');
  Logger.Debug('');

  SoundData := PSndVIOUserData(user_data);

  Logger.Debug('Offset - ' + IntToStr(offset));
  case whence of
    SEEK_CUR:
    begin
      Logger.Debug('SEEK_CUR');
      SoundData^.Position := (SoundData^.Position) + offset;
    end;
    SEEK_SET:
    begin
      Logger.Debug('SEEK_SET');
      SoundData^.Position := offset;
    end;
    SEEK_END:
    begin
      Logger.Debug('SEEK_END');
      SoundData^.Position := (SoundData^.Position) - offset;
    end;
  end;
  Result := SoundData^.Position;

  Logger.Debug('');
  Logger.Debug('Exiting sf_vio_seek_impl');
end;

function sf_vio_read_impl(ptr: pointer; Count: sf_count_t;
  user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
  Position, ActualCount: sf_count_t;
begin
  Logger.Debug('Entering sf_vio_read_impl');
  Logger.Debug('');

  SoundData := PSndVIOUserData(user_data);

  Logger.Debug('Count - ' + IntToStr(Count));
  Logger.Debug('Size - ' + IntToStr(SoundData^.Sound^.Size));

  Position := SoundData^.Position;
  Logger.Debug('Position - ' + IntToStr(SoundData^.Position));
  { Check if this fetch will go beyond the total size of the file buffer }
  if Position >= SoundData^.Sound^.Size then
    ActualCount := 0
  else if Position > (SoundData^.Sound^.Size - Count) then
    ActualCount := (SoundData^.Sound^.Size) - Position
  else
    ActualCount := Count;

  Move(SoundData^.Sound^.Buffer[SoundData^.Position], ptr^, ActualCount);
  SoundData^.Position := (SoundData^.Position) + ActualCount;
  Result := ActualCount;

  Logger.Debug('New position - ' + IntToStr(SoundData^.Position));

  Logger.Debug('');
  Logger.Debug('Exiting sf_vio_read_impl');
end;

function sf_vio_write_impl(const {%H-}ptr: pointer; {%H-}Count: sf_count_t;
  {%H-}user_data: pointer): sf_count_t; cdecl;
begin
  Logger.Debug('Entering sf_vio_write_impl');
  Result := 0;
  Logger.Debug('Exiting sf_vio_write_impl');
end;

function sf_vio_tell_impl(user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
begin
  Logger.Debug('Entering sf_vio_tell_impl');
  Logger.Debug('');

  SoundData := PSndVIOUserData(user_data);
  Result := SoundData^.Position;

  Logger.Debug('Position - ' + IntToStr(SoundData^.Position));
  Logger.Debug('');
  Logger.Debug('Exiting sf_vio_tell_impl');
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
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

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

  Logger.Debug(IntToHex(FInfo.format, 8));
  Logger.Debug(IntToStr(FInfo.channels));
  Logger.Debug(IntToStr(FInfo.frames));
  Logger.Debug(IntToStr(FInfo.samplerate));
  Logger.Debug(IntToStr(FInfo.sections));

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

  FVIOUserData.Sound := @TAudio.DefaultSound;
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
    Logger.Debug('Sf_seek returned error');
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
  Result := (readCount = (frameCount * FInfo.channels));
end;

procedure TSndSound.LoadInMemorySound(SoundData: PSoundData);
var
  TempSoundFile: PSNDFILE;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

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

  Logger.Debug('Default audio loaded.');
  Logger.Debug('');

  Logger.Debug('Format - ' + IntToHex(FInfo.format, 8));
  Logger.Debug('Channels - ' + IntToStr(FInfo.channels));
  Logger.Debug('Frames - ' + IntToStr(FInfo.frames));
  Logger.Debug('Sample Rate - ' + IntToStr(FInfo.samplerate));
  Logger.Debug('Sections - ' + IntToStr(FInfo.sections));
  Logger.Debug('');

  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //LeaveCriticalSection(AudioCriticalSection);

end;

procedure TSndSound.LoadDefaultSound;
begin
  LoadInMemorySound(@TAudio.DefaultSound);
end;

procedure TSndSound.LoadTick;
begin
  LoadInMemorySound(@TAudio.DefaultTick);
end;

{ TAudio }

procedure TAudio.Play(Sound: TSound; PlayLooped: boolean; Volume: integer);
var
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;
  DeviceId: integer;
  AmpScale: double;
begin
  //EnterCriticalSection(AudioCriticalSection);

  try
    if not TAudio.Loaded then
      raise EAudioNotLoaded.Create('Audio not loaded.');

    if FAudioPlaying then
    begin
      ShowMessage('Again?');
      //LeaveCriticalsection(AudioCriticalSection);
      Exit;
    end;

    {if sf_seek(FSoundFile, 0, SEEK_SET) = -1 then
    begin
      Logger.Debug('Sf_seek returned error');
    end;}
    Sound.SeekToBeginning;

    if UseDefaultDevice or (FOutputDevice.HostAPIName = '') or
      (FOutputDevice.DeviceName = '') then
    begin
      DeviceId := DefaultDeviceIndex;
      Logger.Debug('TAudio using default device to play audio.');
    end
    else
    begin
      DeviceId := GetDeviceIndex(FOutputDevice);
      Logger.Debug('TAudio using device - ' + FOutputDevice.DeviceName +
        ' host api - ' + FOutputDevice.HostAPIName);
    end;
    StreamParams.device := DeviceId;

    Logger.Debug('Audio device is ' + IntToStr(StreamParams.device));

    //StreamParams.channelCount := FInfo.channels;
    StreamParams.channelCount := Sound.Channels;
    StreamParams.sampleFormat := Sound.SampleFormat;

    Streamparams.suggestedLatency :=
      Pa_GetDeviceInfo(StreamParams.device)^.defaultLowOutputLatency;
    StreamParams.hostApiSpecificStreamInfo := nil;
    //Logger.Debug('Default device is ' + IntToStr(StreamParams.device));

    //FUserInfo.SoundFile := FSoundFile;

    FUserInfo.Sound := Sound;
    FUserInfo.Looped := PlayLooped;
    FUserInfo.Player := Self;

    Assert((Volume >= 0) and (Volume <= MAX_VOLUME));
    AmpScale := Volume / MAX_VOLUME;
    FUserInfo.Volume := (power(VOLUME_LOG_BASE, AmpScale) - 1) / (VOLUME_LOG_BASE - 1);
    Assert((FUserInfo.Volume >= 0) and (FUserInfo.Volume <= 1));

    //Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

    PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams,
      Sound.SampleRate, paFramesPerBufferUnspecified, paClipOff,
      PPaStreamCallback(@FeedAudioStream), @FUserInfo);
    if (paErrCode <> Int32(paNoError)) then
    begin
      Logger.Debug('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
      Logger.Debug('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    PaErrCode := Pa_SetStreamFinishedCallback(FStream,
      PPaStreamFinishedCallback(@AudioStreamFinished));
    if (paErrCode <> Int32(paNoError)) then
    begin
      Logger.Debug('Pa_SetStreamFinishedCallback failed ' + Pa_GetErrorText(paErrCode));
      Logger.Debug('Error after Pa_SetStreamFinishedCallback ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    PaErrCode := Pa_StartStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      Logger.Debug('Pa_StartStream failed ' + Pa_GetErrorText(paErrCode));
      Logger.Debug('Error after Pa_StartStream ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    FAudioPlaying := True;

  finally
    //LeaveCriticalSection(AudioCriticalSection);
  end;
end;

class procedure TAudio.LoadSoundFromResource(ResourceName: string;
  var Sound: TSoundData);
var
  BytesRead: integer;
  Size: integer;
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(hinstance, ResourceName, RT_RCDATA);
  Size := Stream.Size;
  Assert(Size > 0);
  if Size <= 0 then
    Logger.Debug('Stream.Size is ' + IntToStr(Size) + ' at TAudio.LoadDefaultSounds'
      );

  Sound.Buffer := AllocMem(Size);
  Sound.Size := Size;
  BytesRead := 0;
  BytesRead := Stream.Read(Sound.Buffer^, Size);

  if BytesRead <> Size then
    Logger.Debug('BytesRead does not match Size in TAudio.LoadDefaultSounds');

  Logger.Debug('Size of the stream is ' + IntToStr(Size));
  Sound.Loaded := True;
  Stream.Destroy;
end;

class function TAudio.GetDevices: TAudioDeviceList; static;
var
  NumDevices, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
  DeviceName: string;
  Device: PAudioDevice;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
  begin
    //LeaveCriticalSection(AudioCriticalSection);
    raise EAudioNotLoaded.Create('Audio not loaded.');
  end;

  for Device in TAudio.FDevices do
  begin
    Dispose(Device);
  end;
  TAudio.FDevices.Clear;
  NumDevices := Pa_GetDeviceCount();
  if NumDevices < 0 then
  begin
    Logger.Debug('Pa_GetDeviceCount failed ');
    Logger.Debug('Error after Pa_GetDeviceCount ' + IntToStr(NumDevices));
  end;

  Logger.Debug('Enumerating devices:-');
  Logger.Debug('');
  for Count := 0 to NumDevices - 1 do
  begin
    DeviceInfo := Pa_GetDeviceInfo(Count);
    if DeviceInfo = nil then
    begin
      Logger.Debug('Error after GetDeviceInfo for device #' + IntToStr(Count));
    end
    else
    begin
      if DeviceInfo^.maxOutputChannels > 0 then
      begin
        DeviceName := StrPas(DeviceInfo^.Name);
        HostAPIInfo := Pa_GetHostApiInfo(DeviceInfo^.hostApi);

        if HostAPIInfo = nil then
        begin
          Logger.Debug('Error in getting HostAPI for devide #' +
            IntToStr(Count) + ' (' + DeviceName + ')');
        end;
        Device := New(PAudioDevice);

        Device^.DeviceName := Devicename;
        Device^.HostAPIName := HostAPIInfo^.Name;

        FDevices.Add(Device);
        Logger.Debug('Devide ID - ' + IntToStr(Count));
        Logger.Debug('Device name - ' + DeviceInfo^.Name);
        Logger.Debug('Host API name - ' + HostAPIInfo^.Name);
        Logger.Debug('Device output channels - ' +
          IntToStr(DeviceInfo^.maxOutputChannels));
      end;

    end;

  end;
  Logger.Debug('');
  Result := FDevices;

  //LeaveCriticalSection(AudioCriticalSection);
end;

class procedure TAudio.GetDefaultDevice(Device: PAudioDevice); static;
var
  DevideId: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
  DeviceName, HostAPIName: string;
begin
  //EnterCriticalSection(AudioCriticalSection);
  DevideId := TAudio.GetDefaultDeviceIndex;
  DeviceInfo := Pa_GetDeviceInfo(DevideId);
  if DeviceInfo = nil then
  begin
    Logger.Debug('Error after GetDeviceInfo for device #' + IntToStr(DevideId));
    Exit;
  end;

  DeviceName := StrPas(DeviceInfo^.Name);
  HostAPIInfo := Pa_GetHostApiInfo(DeviceInfo^.hostApi);
  if HostAPIInfo = nil then
  begin
    Logger.Debug('Could not get HostAPI details');
    Exit;
  end;
  HostAPIName := StrPas(HostAPIInfo^.Name);

  Device^.DeviceName := DeviceName;
  Device^.HostAPIName := HostAPIName;

  //LeaveCriticalSection(AudioCriticalSection);
end;

class procedure TAudio.SetOutputDevice(AValue: TAudioDevice);
begin
  try
    FOutputDevice.DeviceName := AValue.DeviceName;
    FOutputDevice.HostAPIName := AValue.HostAPIName;
  finally
  end;
end;

{procedure TAudio.SetVolume(AValue: integer);
begin
  if FVolume=AValue then Exit;
  FVolume:=Min(AValue, MAX_VOLUME);
end;}

constructor TAudio.Create;
var
  i: integer;
begin

  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  Looped := False;
  FAudioPlaying := False;

  OnPlayCompletion := nil;

  { Fill a Sine wavetable (Float Data -1 .. +1) }
  for i := 0 to TableSize - 1 do
    Data.Sine[i] := CFloat((Sin((CFloat(i) / CFloat(TableSize)) * Pi * 2)));

  Data.LeftPhase := 0;
  Data.RightPhase := 0;
  Data.AMessage := 'No Message'#0;

  DataPointer := @Data;

end;

destructor TAudio.Destroy;
begin
  inherited Destroy;
end;

class function TAudio.GetDefaultDeviceIndex: AudioDeviceIndex;
var
  DeviceId: AudioDeviceIndex;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  DeviceId := Pa_GetDefaultOutputDevice();
  if DeviceId = paNoDevice then
  begin
    Logger.Debug('No default device');
    //LeaveCriticalSection(AudioCriticalSection);
    raise EInvalidDevice.Create('Pa_GetDefaultOutputDevice() returned PaNoDevice');
  end;
  Result := DeviceId;
  //LeaveCriticalSection(AudioCriticalSection);
end;

class function TAudio.GetDeviceIndex(Device: TAudioDevice): AudioDeviceIndex;
var
  NumDevices, CountDevice: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  NumDevices := Pa_GetDeviceCount();
  if NumDevices < 0 then
  begin
    Logger.Debug('Pa_GetDeviceCount failed ');
    Logger.Debug('Error after Pa_GetDeviceCount ' + IntToStr(NumDevices));
  end;

  for CountDevice := 0 to NumDevices - 1 do
  begin
    DeviceInfo := Pa_GetDeviceInfo(CountDevice);
    if DeviceInfo = nil then
    begin
      Logger.Debug('Error after GetDeviceInfo for device #' + IntToStr(CountDevice));
      Continue;
    end;

    if Device.DeviceName = StrPas(DeviceInfo^.Name) then
    begin
      HostAPIInfo := Pa_GetHostApiInfo(DeviceInfo^.hostApi);
      if HostAPIInfo = nil then
      begin
        Logger.Debug('Error after Pa_GetHostApiInfo for device #' +
          IntToStr(CountDevice) + ' host api #' + IntToStr(DeviceInfo^.hostApi));
        Continue;
      end;
      if HostAPIInfo^.Name = Device.HostAPIName then
      begin
        Result := CountDevice;
        //LeaveCriticalSection(AudioCriticalSection);
        Exit;
      end;
    end;
  end;
  //LeaveCriticalSection(AudioCriticalSection);
  raise EInvalidDevice.Create('No matching device for ' + Device.DeviceName +
    ':' + Device.HostAPIName);

end;

class procedure TAudio.CleanUp;
var
  Device: PAudioDevice;
begin
  if not TAudio.Loaded then
  begin
    Logger.Debug('TAudio.Loaded is fales in TAudio.Cleanup');
    Exit;
  end;

  for Device in TAudio.FDevices do
  begin
    Dispose(Device);
  end;
  TAudio.FDevices.Clear;

  FreeMem(TAudio.DefaultSound.Buffer);
  FreeMem(TAudio.DefaultTick.Buffer);
end;

class procedure TAudio.SetDefaulDevice;
begin
  TAudio.GetDefaultDevice(@FOutputDevice);
end;

class procedure TAudio.LoadDefaultSounds;
begin
  if not TAudio.Loaded then
  begin
    Exit;
  end;
  LoadSoundFromResource('DEFAULT_SOUND', TAudio.DefaultSound);
  LoadSoundFromResource('TICK', TAudio.DefaultTick);
end;

class procedure TAudio.FreeDefaultSounds;
begin
  FreeMem(DefaultSound.Buffer);
end;

procedure TAudio.Abort;
var
  PaErrCode: PaError;
begin
  //EnterCriticalSection(AudioCriticalSection);
  try
    if not TAudio.Loaded then
      raise EAudioNotLoaded.Create('Audio not loaded.');

    if not FAudioPlaying then
    begin
      Logger.Debug('Audio not playing. There is nothing to be done in abort');
      Exit;
    end;
    PaErrCode := Pa_AbortStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      Logger.Debug('Pa_AbortStream failed ' + Pa_GetErrorText(paErrCode));
      Logger.Debug('Error after Pa_AbortStream ' + IntToHex(PaErrCode, 8));
    end;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}

  finally
    //FAudioPlaying := False;
    //LeaveCriticalSection(AudioCriticalSection);
  end;

end;

procedure TAudio.FinishedAud(Datax: PtrInt);
var
  PaErrCode: PaError;
begin
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  //EnterCriticalSection(AudioCriticalSection);

  {This check might be redundant. Just to be safe}
  Assert(FStream <> nil);
  {paErrCode := Pa_IsStreamStopped(FStream);
  if paErrCode = 0 then
  begin
    paErrCode := Pa_StopStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      Logger.Debug('Pa_StopStream failed ' + Pa_GetErrorText(paErrCode));
      Logger.Debug('Error after Pa_StopStream ' + IntToHex(PaErrCode, 8));
    end;
  end
  else if PaErrCode <> 1 then
  begin
    Logger.Debug('Pa_IsStreamStopped failed ' + Pa_GetErrorText(paErrCode));
    Logger.Debug('Error after Pa_IsStreamStopped ' + IntToHex(PaErrCode, 8));
  end;}

  paErrCode := Pa_StopStream(FStream);
  if (paErrCode <> Int32(paNoError)) then
  begin
    Logger.Debug('Pa_StopStream failed ' + Pa_GetErrorText(paErrCode));
    Logger.Debug('Error after Pa_StopStream ' + IntToHex(PaErrCode, 8));
  end;

  paErrCode := Pa_CloseStream(FStream);
  if (paErrCode <> Int32(paNoError)) then
  begin
    Logger.Debug('Pa_CloseStream failed ' + Pa_GetErrorText(paErrCode));
    Logger.Debug('Error after Pa_CloseStream ' + IntToHex(PaErrCode, 8));
  end;
  FStream := nil;
  FAudioPlaying := False;
  if OnPlayCompletion <> nil then
    OnPlayCompletion(Self);
  //LeaveCriticalSection(AudioCriticalSection);
end;

class function TAudio.LoadSound(Avalue: string): TSound;
var
  SndSound: TSndSound;
  MpgSound: TMpgSound;
begin
  SndSound := TSndSound.Create;
  SndSound.SetFileName(AValue);
  if SndSound.FileName <> '' then
  begin
    Assert(SndSound <> nil);

    Result := SndSound;
    Exit;
  end;
  SndSound.Free;

  MpgSound := TMpgSound.Create;
  MpgSound.SetFileName(AValue);
  if MpgSound.FileName <> '' then
  begin
    Assert(MpgSound <> nil);
    Result := MpgSound;

    Exit;
  end;
  MpgSound.Free;

  raise EInvalidAudio.Create('sndfile & mpg123 returned error for ' + AValue);
end;

var
  PaErrCode: PaError;


initialization
  TAudio.Loaded := False;
  TAudio.DefaultSound.Loaded := False;
  TAudio.DefaultTick.Loaded := False;
  TAudio.FDevices := TAudioDeviceList.Create;

  {$IFNDEF AUDIO_STATIC}
  TAudio.Loaded := Pa_Load(LIB_PORTAUDIO);
  if not TAudio.Loaded then
  begin
    Logger.Debug('Could not load portaudio');
    Exit;
  end;

  { Load sndfile library only if portaudio was loaded successfully }

  if TAudio.Loaded then
  begin
    TAudio.Loaded := sf_load(LIB_SNDFILE);
    if not TAudio.Loaded then
    begin
      Logger.Debug('Could not load sndfile');
      Pa_Unload();
      Exit;
    end;
  end;

  if TAudio.Loaded then
  begin
    TAudio.Loaded := Mp_Load(LIB_MPG123);
    if not TAudio.Loaded then
    begin
      Logger.Debug('Could not load mpg123');
      sf_Unload;
      Pa_Unload;
      Exit;
    end;
    if mpg123_init() <> MPG123_OK then
    begin
      TAudio.Loaded := False;
      Logger.Debug('mpg123_init() failed');
      Mp_Unload;
      sf_Unload;
      Pa_Unload;
      Exit;
    end;
  end;

  //FAudioWorking:=Status;

  {$ENDIF}

  { If everything has gone alright so far, attempt to initialise
  PortAudio }

  if TAudio.Loaded then
  begin
    PaErrCode := Pa_Initialize();
    if PaErrCode <> cint(paNoError) then
    begin
      Logger.Debug('Error in Pa_Initialize()');

      TAudio.Loaded := False;

      { If portaudio cannot be initialised, then audio will not work.
      Unload libraries }
      {$IFNDEF AUDIO_STATIC}
      Mp_Unload;
      sf_Unload;
      Pa_Unload;

      {$ENDIF}
    end;
  end;

  if TAudio.Loaded then
  begin
    TAudio.FDefaultDevice := Pa_GetDefaultOutputDevice();
    if TAudio.FDefaultDevice = paNoDevice then
    begin
      Logger.Debug('No default device');
      TAudio.Loaded := False;
      Pa_Terminate;
      {$IFNDEF AUDIO_STATIC}
      Mp_Unload;
      sf_Unload;
      Pa_Unload;
      {$ENDIF}
    end;
  end;
  //InitCriticalSection(TAudio.AudioCriticalSection);


finalization
  //DoneCriticalsection(TAudio.AudioCriticalSection);
  if TAudio.Loaded then
  begin
    Pa_Terminate;
    mpg123_exit;
    {$IFNDEF AUDIO_STATIC}
    Mp_Unload;
    Sf_Unload;
    Pa_Unload;
    {$ENDIF}
  end;
  TAudio.CleanUp;
  TAudio.FDevices.Free;
end.
