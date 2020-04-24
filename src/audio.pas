unit audio;

{$mode objfpc}{$H+}

// We do not want stuck-up, toffee-nosed, half-witted,
// upper-class piles of COM-style interfaces.

{$interfaces corba}
//{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, sndfile, mpg123, portaudio, LazLogger, ctypes, Forms,
  Dialogs, LCLIntf, lcltype, fgl;

const
  READ_NOTLOADED = -1;
  READ_SND = 0;

  SampleRate = 44100;
  FramesPerBuffer = 64;

  // How long you want to play the test sine:
  NumSecs = 2;

  // Wavetable size. Influences your pitch:
  TableSize = 200;

  SAudioFile = '{6decd475-7e30-461a-989c-995bb233ad7a}';

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

  { TAudioFile }

  TAudioFile = class(TObject)
    //[SAudioFile]
    private
      function GetChannels: Integer; virtual; abstract;
      function GetSampleRate: Integer;virtual; abstract;
      function GetAudioLength: double;virtual; abstract;
      function GetSampleFormat: PaSampleFormat; virtual; abstract;
      function GetSource: string; virtual; abstract;
    public
      constructor Create;
      destructor Destroy; override;

    property Channels: Integer read GetChannels;
    property SampleRate: Integer read GetSampleRate;
    property Duration: double read GetAudioLength;
    property SampleFormat: PaSampleFormat read GetSampleFormat;
    property Source: string read GetSource;
    // Seek to begin
    procedure SeekToBeginning; virtual; abstract;
    // Read to buffer
    function Read(output: pointer; frameCount: LongInt): boolean; virtual; abstract;
  end;

  { The record passed to port audio callbacks }
  TUserInfo = record

    //SoundFile: PSndFile;
    //Info: SF_INFO;
    AudioFile: TAudioFile;
    Player: Pointer;
    Looped: boolean;

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


  {TSndAudioFile}
  {Implementation of SndFile}
  TSndAudioFile = class(TAudioFile)
  private
    FSoundFile: PSNDFILE;
    //FFileType: integer;

    FFileName: string;
    FAudioLength: double;

    FInfo: SF_INFO;

    FVirtualIOCallbacks: SF_VIRTUAL_IO;
    FVIOUserData: TSndVIOUserData;

    function GetAudioLength: double; override;
    function GetChannels: Integer;override;
    function GetSampleFormat: PaSampleFormat; override;
    function GetSampleRate: Integer; override;
    procedure SetFileName(AValue: string);
    function GetSource: string; override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure SeekToBeginning; override;
    function Read(output: pointer; frameCount: LongInt): boolean; override;

    property FileName: string read FFileName write SetFileName;
    procedure LoadDefaultSound;
    property Channels: Integer read GetChannels;
    property SampleRate: Integer read GetSampleRate;
    property Duration: double read GetAudioLength;
    property SampleFormat: PaSampleFormat read GetSampleFormat;
    property Source: string read GetSource;
  end;

  {TMpgAudioFile}
  {Implementation of Mpg123 for MP3}
  TMpgAudioFile = class(TAudioFile)
  private
    FHandle: Tmpg123_handle;
    FError: integer;
    //FFileType: integer;

    FFileName: string;
    FAudioLength: double;

    FRate: cardinal;
    FChannelCount: integer;
    FEncoding: integer;

    function GetAudioLength: double;override;
    function GetChannels: Integer;override;
    function GetSampleFormat: PaSampleFormat; override;
    function GetSampleRate: Integer; override;
    procedure SetFileName(AValue: string);
    function GetSource: string; override;

  public
    constructor Create();
    destructor Destroy; override;
    procedure SeekToBeginning; override;
    function Read(output: pointer; frameCount: LongInt): boolean; override;

    property FileName: string read FFileName write SetFileName;
    property Channels: Integer read GetChannels;
    property SampleRate: Integer read GetSampleRate;
    property Duration: double read GetAudioLength;
    property SampleFormat: PaSampleFormat read GetSampleFormat;
    property Source: string read GetSource;
  end;

  { TAudio }
  TAudio = class(TObject)
  private
    //FSoundFile: PSNDFILE;
    FAudioFile: TAudioFile;

    FSndAudioFile: TSndAudioFile;
    FMpgAudioFile: TMpgAudioFile;

    FDefAudioFile: TSndAudioFile;
    FTickAudioFile: TSndAudioFile;

    //FSndDefaultSoundFile: TSndAudioFile;
    //FSndAudioF
    //FFileType: integer;

    FFileName: string;
    //FAudioFileLoaded: boolean;
    //FAudioLength: double;

    //FInfo: SF_INFO;


    FUserInfo: TUserInfo;
    FStream: PPaStream;

    FAudioPlaying: boolean;

    Data: PaTestData;
    DataPointer: PPaTestData;
    function GetAudioFileLoaded: boolean;
    function GetDuration: double;
    class procedure LoadSoundFromResource(ResourceName: string; var Sound: TSoundData); static;


    class function GetDevices: TAudioDeviceList; static;
    //procedure SetFileName(AValue: string);
    class procedure SetOutputDevice(AValue: TAudioDevice); static;
  public
    Loaded: boolean; static;
    UseDefaultDevice: boolean; static;
    FDevices: TAudioDeviceList; static;
    FDefaultDevice: integer; static;
    AudioCriticalSection: TRTLCriticalSection; static;
    FOutputDevice: TAudioDevice; static;

    {Default sounds }
    DefaultSound: TSoundData; static;
    DefaultTick: TSoundData; static;

    OnPlayCompletion: TNotifyEvent;
    Looped: boolean;
    constructor Create();
    destructor Destroy; override;
    class function GetDefaultDeviceIndex: AudioDeviceIndex; static;
    class procedure GetDefaultDevice(Device: PAudioDevice); static;
    class function GetDeviceIndex(Device: TAudioDevice): AudioDeviceIndex; static;
    class property DefaultDeviceIndex: AudioDeviceIndex read GetDefaultDeviceIndex;
    class property Devices: TAudioDeviceList read GetDevices;
    class procedure CleanUp; static;
    class procedure SetDefaulDevice; static;
    class procedure LoadDefaultSounds; static;
    class procedure FreeDefaultSounds; static;
    procedure Play(AudioFile: TAudioFile; PlayLooped: boolean = False);
    procedure Play;
    procedure PlayDefaultSound;
    procedure PlayTickSound;
    procedure PlayTest;
    procedure PlaySine;
    procedure Abort;
    //procedure SetDefaultSound;
    procedure FinishedAud({%H-}Datax: PtrInt);
    property FileName: string read FFileName;
    procedure UnloadAudioFile;
    procedure LoadFromFile(AValue: string);
    property Duration: double read GetDuration;
    property Playing: boolean read FAudioPlaying;
    //property AudioFile: TAudioFile read FAudioFile;
    property AudioFileLoaded: boolean read GetAudioFileLoaded;
    class property OutputDevice: TAudioDevice read FOutputDevice write SetOutputDevice;

  end;


implementation

{class operator TAudioDevice.=(aLeft, aRight: TAudioDevice): Boolean;
begin
   Result := (aLeft.Device = aRight.Device) and (aLeft.HostAPI = aRight.HostAPI);
end;}
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
  //readCount: cint;
  readSuccess: boolean;
  AudioInstance: TAudio;
  UsedAudioFile: TAudioFile;
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);

  AudioInfo := PAudioinfo(userData);
  AudioInstance := TAudio(AudioInfo^.Player);

  Assert(AudioInfo^.AudioFile <> nil);

  //if AudioInfo^.AudioFile <> nil then
    UsedAudioFile:=AudioInfo^.AudioFile;
  //else
  //  UsedAudioFile:=AudioInstance.AudioFile;

  //readCount := 0;
  readSuccess := false;
  //readCount := sf_read_float(AudioInfo^.SoundFile, output, frameCount *
  //  (AudioInfo^.Info.channels));
  readSuccess:=UsedAudioFile.Read(output, frameCount);
  //DebugLn('here 1');
  if AudioInfo^.Looped then
  begin
    { If audio is lopped and if no audio data could be read,
    seek to the beginning and then read again }
    if not readSuccess then
    begin
      {if sf_seek(AudioInfo^.SoundFile, 0, SEEK_SET) = -1 then
      begin
        DebugLn('Sf_seek returned error');
      end;}
      UsedAudioFile.SeekToBeginning;
      //readCount := 0;
      readSuccess:=false;
      //readCount := sf_read_float(AudioInfo^.SoundFile, output, frameCount *
      //  (AudioInfo^.Info.channels));
      readSuccess:=UsedAudioFile.Read(output, frameCount);
      if not readSuccess then
      begin
        DebugLn('readCount zero immediately after seek to beginning');
        Result := cint(paAbort);
        LeaveCriticalSection(TAudio.AudioCriticalSection);
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

  LeaveCriticalSection(TAudio.AudioCriticalSection);
end;

{This function is called by PortAudio to signal that the stream has stopped.
As this is a non-class function, it sends a message to frame using the frame's
handle.}
procedure AudioStreamFinished(UserData: pointer); cdecl;
var
  AudioInfo: PAudioInfo;
  {%H-}AudioTemp: TAudio; // Possibly buggy hint, omitting
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);

  AudioInfo := PAudioinfo(userData);

  AudioTemp := TAudio(AudioInfo^.Player);

  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  LeaveCriticalSection(TAudio.AudioCriticalSection);
end;

{ The callback function which is called by PA everytime new Data is needed.
  Remember: ALWAYS USE CDECL or your pointers will be messed up!
  Pointers to this function must be castable to PPaStreamCallback: }
function PaTestCallback({%H-}inputBuffer: pointer; OutputBuffer: pointer;
  framesPerBuffer: culong; {%H-}timeInfo: PPaStreamCallbackTimeInfo;
  {%H-}statusFlags: PaStreamCallbackFlags; UserData: pointer): CInt32;
  cdecl;
var
  OutBuffer: PCFloat;
  i: culong;
  LocalDataPointer: PPaTestData;
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);
  OutBuffer := PCFloat(OutputBuffer);
  LocalDataPointer := PPaTestData(UserData);

  // Fill the buffer...
  for i := 0 to (FramesPerBuffer - 1) do
  begin

    OutBuffer^ := LocalDataPointer^.Sine[LocalDataPointer^.LeftPhase];
    Inc(OutBuffer);

    OutBuffer^ := LocalDataPointer^.Sine[LocalDataPointer^.RightPhase];
    Inc(OutBuffer);

    Inc(LocalDataPointer^.LeftPhase);
    if (LocalDataPointer^.LeftPhase >= TableSize) then
      LocalDataPointer^.LeftPhase :=
        (LocalDataPointer^.LeftPhase - TableSize);

    Inc(LocalDataPointer^.RightPhase);
    Inc(LocalDataPointer^.RightPhase);
    if (LocalDataPointer^.RightPhase >= TableSize) then
      LocalDataPointer^.RightPhase :=
        (LocalDataPointer^.RightPhase - TableSize);
  end;

  Result := CInt32(0);
  LeaveCriticalSection(TAudio.AudioCriticalSection);
end;


{ This is called when playback is finished.
  Remember: ALWAYS USE CDECL or your pointers will be messed up!
  Pointers to this function must be castable to PPaStreamFinishedCallback: }
procedure StreamFinished(UserData: pointer); cdecl;
var
  LocalDataPointer: PPaTestData;
  {%H-}AudioTemp: TAudio; // buggy hint, omitting
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);
  LocalDataPointer := PPaTestData(UserData);
  AudioTemp := TAudio(LocalDataPointer^.Player);
  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  LeaveCriticalSection(TAudio.AudioCriticalSection);
end;

function sf_vio_get_filelen_impl(user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
begin
  DebugLn('Entering sf_vio_get_filelen_impl');
  SoundData := PSndVIOUserData(user_data);
  Result:=SoundData^.Sound^.Size;
  DebugLn('Return value is ' + IntToStr(SoundData^.Sound^.Size));
  DebugLn('Exiting sf_vio_get_filelen_impl');
end;
function sf_vio_seek_impl(offset: sf_count_t; whence: cint;
    user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
begin
  DebugLn('Entering sf_vio_seek_impl');
  DebugLnEnter;

  SoundData := PSndVIOUserData(user_data);

  DebugLn('Offset - ' + IntToStr(offset));
  case whence of
    SEEK_CUR:
      begin
        DebugLn('SEEK_CUR');
        SoundData^.Position:=(SoundData^.Position)+offset;
      end;
    SEEK_SET:
      begin
        DebugLn('SEEK_SET');
        SoundData^.Position:=offset;
      end;
    SEEK_END:
    begin
      DebugLn('SEEK_END');
      SoundData^.Position:=(SoundData^.Position)-offset;
    end;
  end;
  Result:=SoundData^.Position;

  DebugLnExit;
  DebugLn('Exiting sf_vio_seek_impl');
end;
function sf_vio_read_impl(ptr: pointer; Count: sf_count_t;
  user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
  Position, ActualCount: sf_count_t;
begin
  DebugLn('Entering sf_vio_read_impl');
  DebugLnEnter;

  SoundData := PSndVIOUserData(user_data);

  DebugLn('Count - ' + IntToStr(Count));
  DebugLn('Size - ' + IntToStr(SoundData^.Sound^.Size));

  Position := SoundData^.Position;
  DebugLn('Position - ' + IntToStr(SoundData^.Position));
  { Check if this fetch will go beyond the total size of the file buffer }
  if Position >=  SoundData^.Sound^.Size then
    ActualCount:=0
  else if Position > (SoundData^.Sound^.Size - Count) then
    ActualCount:=(SoundData^.Sound^.Size) - Position
  else
    ActualCount:=Count;

  Move(SoundData^.Sound^.Buffer[SoundData^.Position], ptr^, ActualCount);
  SoundData^.Position:=(SoundData^.Position)+ActualCount;
  Result:=ActualCount;

  DebugLn('New position - ' + IntToStr(SoundData^.Position));

  DebugLnExit;
  DebugLn('Exiting sf_vio_read_impl');
end;

function sf_vio_write_impl(const ptr: pointer; Count: sf_count_t;
  user_data: pointer): sf_count_t; cdecl;
begin
  DebugLn('Entering sf_vio_write_impl');
  Result:=0;
  DebugLn('Exiting sf_vio_write_impl');
end;

function sf_vio_tell_impl(user_data: pointer): sf_count_t; cdecl;
var
  SoundData: PSndVIOUserData;
begin
  DebugLn('Entering sf_vio_tell_impl');
  DebugLnEnter;

  SoundData := PSndVIOUserData(user_data);
  Result:=SoundData^.Position;

  DebugLn('Position - ' + IntToStr(SoundData^.Position));
  DebugLnExit;
  DebugLn('Exiting sf_vio_tell_impl');
end;

{ TAudioFile }

constructor TAudioFile.Create;
begin
  //inherited Create;
end;

destructor TAudioFile.Destroy;
begin
  inherited Destroy;
end;

{ TMpgAudioFile }

function TMpgAudioFile.GetAudioLength: double;
begin
  Result:=FAudioLength;
end;

function TMpgAudioFile.GetChannels: Integer;
begin
  Result:=FChannelCount;
end;

function TMpgAudioFile.GetSampleFormat: PaSampleFormat;
begin
  //Result:=paInt16;
  Result:=paFloat32;
end;

function TMpgAudioFile.GetSampleRate: Integer;
begin
  Result:=FRate;
end;

procedure TMpgAudioFile.SetFileName(AValue: string);
var
  Dur: double;
  FrameLength: coff_t;
begin
  FError := mpg123_close(FHandle);
  if FError <> MPG123_OK then
  begin
    DebugLn('Error after mpg123_close ' + IntToStr(FError));
    //Exit;
  end;

  FError := mpg123_open(FHandle, PChar(AValue));
  if FError <> MPG123_OK then
  begin
    DebugLn('Error after mpg123_open ' + IntToStr(FError));
    //raise EInvalidAudio.Create('mpg123_open returned error for ' + AValue);
    FFileName:='';
    Exit;
  end;
  FFileName:=AValue;
  DebugLn('MPEG audio file loaded: ' + FFileName);
  FError := mpg123_getformat(FHandle, FRate, FChannelCount, FEncoding);
  if FError <> MPG123_OK then
  begin
    //raise EInvalidAudio.Create('mpg123_getformat returned error for ' + AValue);

    FFileName:='';
    Exit;
  end;
  DebugLnEnter;
  DebugLn('Rate - ' + IntToStr(FRate));
  DebugLn('Encoding - ' + IntToStr(FEncoding));

  FError:=mpg123_scan(FHandle);
  if FError <> MPG123_OK then
  begin
    DebugLn('Error after mpg123_scan ' + IntToStr(FError));
  end;

  FrameLength := mpg123_length(FHandle);
  DebugLn('FrameLength is ' + IntToStr(FrameLength));
  if FrameLength < 0 then
  begin
    DebugLn('Error after mpg123_scan ' + IntToStr(FrameLength));
  end;

  {Dur :=  mpg123_tpf(FHandle);
  DebugLn('Duration is ' + FloatToStr(Duration));
  DebugLnExit;

  if Dur < 0 then
  begin
    DebugLn('Error after mpg123_tpf ' + FloatToStr(Dur));
  end;}
  Dur := FrameLength / FRate;
  DebugLn('Duration is ' + FloatToStr(Duration));

  FAudioLength:=Dur;
end;

function TMpgAudioFile.GetSource: string;
begin
  Result := FFileName;
end;

constructor TMpgAudioFile.Create();
var
  Rates: plong = nil;
  RateCount: size_t = 0;
  //X, Y: Pointer;
  Count: integer;
begin
  FHandle:=nil;
  FHandle := mpg123_new(nil, FError);
  if FError <> MPG123_OK then
    DebugLn('Error after mpg123_new ' + IntToStr(FError));

  FError := mpg123_format_none(FHandle);
  if FError <> MPG123_OK then
    DebugLn('Error after mpg123_format_none ' + IntToStr(FError));

  //RateCount := 1;
  //X := Addr(Rates);
  //Y := Addr(RateCount);
  //Rates := nil;

  mpg123_rates(Rates, RateCount);

  if Rates <> nil then
    DebugLn('Rates is not nil');

  //DebugLn('After mpg123_rates. Rate count is ' + IntToStr(Rates[0]));


  for Count:=0 to RateCount - 1 do
  begin
    //DebugLn('Handling rate count ' + IntToStr(Rates[Count]));

    mpg123_format(FHandle, Rates[Count], MPG123_MONO or MPG123_STEREO, MPG123_ENC_FLOAT_32);
    if FError <> MPG123_OK then
      DebugLn('Error after mpg123_format ' + IntToStr(FError));

  end;

  //TODO: Remove the hard coding
  mpg123_format(FHandle, 44100, MPG123_MONO or MPG123_STEREO, MPG123_ENC_FLOAT_32);
  if FError <> MPG123_OK then
    DebugLn('Error after mpg123_format ' + IntToStr(FError));

end;

destructor TMpgAudioFile.Destroy;
begin
  FError := mpg123_close(FHandle);
  if FError <> MPG123_OK then
  begin
    DebugLn('Error after mpg123_close ' + IntToStr(FError));
  end;
  if FHandle <> nil then
    mpg123_delete(FHandle);
  inherited Destroy;
end;

procedure TMpgAudioFile.SeekToBeginning;
begin
  FError :=  mpg123_seek(FHandle, 0, SEEK_SET);
end;

function TMpgAudioFile.Read(output: pointer; frameCount: LongInt): boolean;
var
  done: size_t = 0;
begin
  FError := mpg123_read(FHandle, output, frameCount * SizeOf(real), done);
  if done = 0 then
  begin
    Result := false;
    Exit;
  end;
  if FError <> MPG123_OK then
  begin
    //DebugLn('mpg123_read failed?');
    Result := false;
    Exit;
  end;
  Result := True;
end;

{ TSndAudioFile }

procedure TSndAudioFile.SetFileName(AValue: string);
var
  TempSoundFile: PSNDFILE;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  {if FAudioPlaying then
  begin
    DebugLn('Cannot call SetFileName when audio is playing');
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
    DebugLn('Error in sf_open');
    //LeaveCriticalSection(AudioCriticalSection);
    //raise EInvalidAudio.Create('sf_open returned nil for ' + AValue);
    FFileName:='';
    Exit;
  end;

  if FSoundFile <> nil then
    sf_close(FSoundFile);

  FSoundFile := TempSoundFile;

  DebugLn(IntToHex(FInfo.format, 8));
  DebugLn(IntToStr(FInfo.channels));
  DebugLn(IntToStr(FInfo.frames));
  DebugLn(IntToStr(FInfo.samplerate));
  DebugLn(IntToStr(FInfo.sections));

  FFileName := AValue;
  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //LeaveCriticalSection(AudioCriticalSection);
end;

function TSndAudioFile.GetSource: string;
begin
  Result:=FFileName;
end;

function TSndAudioFile.GetChannels: Integer;
begin
  Result:=FInfo.channels;
end;

function TSndAudioFile.GetSampleFormat: PaSampleFormat;
begin
  Result:=paFloat32;
end;

function TSndAudioFile.GetAudioLength: double;
begin
  Result:=FAudioLength;
end;

function TSndAudioFile.GetSampleRate: Integer;
begin
  Result:=FInfo.samplerate;
end;

constructor TSndAudioFile.Create();
begin
  //inherited create;
  FSoundFile:=Nil;
  with FVirtualIOCallbacks do
  begin
    get_filelen:=@sf_vio_get_filelen_impl;
    Read:=@sf_vio_read_impl;
    seek:=@sf_vio_seek_impl;
    tell:=@sf_vio_tell_impl;
    Write:=@sf_vio_write_impl;
  end;

  FVIOUserData.Sound:=@TAudio.DefaultSound;
  FVIOUserData.Position:=0;

end;

destructor TSndAudioFile.Destroy;
begin
  if FSoundFile <> nil then
    sf_close(FSoundFile);
  inherited Destroy;
end;

procedure TSndAudioFile.SeekToBeginning;
begin
  if sf_seek(FSoundFile, 0, SEEK_SET) = -1 then
  begin
    DebugLn('Sf_seek returned error');
  end;
end;

function TSndAudioFile.Read(output: pointer; frameCount: LongInt): boolean;
var
  readCount: cint;
begin
  readCount := 0;
  readCount := sf_read_float(FSoundFile, output, frameCount * FInfo.channels);
  // If read count matches what was requested, then all the stream has
  // not completed
  Result:=(readCount = (frameCount * FInfo.channels));
end;

procedure TSndAudioFile.LoadDefaultSound;
var
  TempSoundFile: PSNDFILE;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  DebugLn('Before calling sf_open_virtual');
  DebugLnEnter;
  DebugLn('Position - ' + IntToStr(FVIOUserData.Position));
  DebugLn('Size - ' + IntToStr(FVIOUserData.Sound^.Size));
  DebugLnExit;

  TempSoundFile := sf_open_virtual(@FVirtualIOCallbacks, SFM_READ, @FInfo, @FVIOUserData);
  if (TempSoundFile = nil) then
  begin
    DebugLn('Error in sf_open_virtual');
    Exit;
  end;
  DebugLn('sf_open_virtual successful');
  if FSoundFile <> nil then
    sf_close(FSoundFile);

  FSoundFile := TempSoundFile;
  FFileName:='';

  DebugLn('Default audio loaded.');
  DebugLnEnter;

  DebugLn('Format - ' + IntToHex(FInfo.format, 8));
  DebugLn('Channels - ' + IntToStr(FInfo.channels));
  DebugLn('Frames - ' + IntToStr(FInfo.frames));
  DebugLn('Sample Rate - ' + IntToStr(FInfo.samplerate));
  DebugLn('Sections - ' + IntToStr(FInfo.sections));
  DebugLnExit;

  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //LeaveCriticalSection(AudioCriticalSection);

end;

{ TAudio }

procedure TAudio.Play(AudioFile: TAudioFile; PlayLooped: boolean);
var
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;
  DeviceId: integer;
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
      DebugLn('Sf_seek returned error');
    end;}
    AudioFile.SeekToBeginning;

    if UseDefaultDevice or (FOutputDevice.HostAPIName = '') or
      (FOutputDevice.DeviceName = '') then
    begin
      DeviceId := DefaultDeviceIndex;
      DebugLn('TAudio using default device to play audio.');
    end
    else
    begin
      DeviceId := GetDeviceIndex(FOutputDevice);
      DebugLn('TAudio using device - ' + FOutputDevice.DeviceName +
        ' host api - ' + FOutputDevice.HostAPIName);
    end;
    StreamParams.device := DeviceId;

    DebugLn('Audio device is ' + IntToStr(StreamParams.device));

    //StreamParams.channelCount := FInfo.channels;
    StreamParams.channelCount:=AudioFile.Channels;
    StreamParams.sampleFormat := AudioFile.SampleFormat;

    Streamparams.suggestedLatency :=
      Pa_GetDeviceInfo(StreamParams.device)^.defaultLowOutputLatency;
    StreamParams.hostApiSpecificStreamInfo := nil;
    //DebugLn('Default device is ' + IntToStr(StreamParams.device));

    //FUserInfo.SoundFile := FSoundFile;

    FUserInfo.AudioFile:=AudioFile;
    FUserInfo.Looped := PlayLooped;
    FUserInfo.Player := Self;

    //Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

    PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams, AudioFile.SampleRate,
      paFramesPerBufferUnspecified, paClipOff, PPaStreamCallback(@FeedAudioStream),
      @FUserInfo);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    PaErrCode := Pa_SetStreamFinishedCallback(FStream,
      PPaStreamFinishedCallback(@AudioStreamFinished));
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_SetStreamFinishedCallback failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_SetStreamFinishedCallback ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    PaErrCode := Pa_StartStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_StartStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_StartStream ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    FAudioPlaying := True;

  finally
    //LeaveCriticalSection(AudioCriticalSection);
  end;
end;

function TAudio.GetDuration: double;
begin
  if not AudioFileLoaded then
  begin
    Result := -1;
    Exit;
  end;
  Assert(FAudioFile <> nil);
  Result:=FAudioFile.Duration;
end;

class procedure TAudio.LoadSoundFromResource(ResourceName: string; var Sound: TSoundData);
var
  BytesRead: integer;
  Size: integer;
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(hinstance, ResourceName, RT_RCDATA);
  Size := Stream.Size;
  Assert(Size > 0);
  if Size <= 0 then
    DebugLn('Stream.Size is ' + IntToStr(Size) + ' at TAudio.LoadDefaultSounds'
      );

  Sound.Buffer:=AllocMem(Size);
  Sound.Size:=Size;
  BytesRead:=0;
  BytesRead := Stream.Read(Sound.Buffer^, Size);

  if BytesRead <> Size then
    DebugLn('BytesRead does not match Size in TAudio.LoadDefaultSounds');

  DebugLn('Size of the stream is ' + IntToStr(Size));
  Sound.Loaded:=True;
  Stream.Destroy;
end;

function TAudio.GetAudioFileLoaded: boolean;
begin
  Result:=(FAudioFile <> nil);
end;

class function TAudio.GetDevices: TAudioDeviceList; static;
var
  NumDevices, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
  DeviceName: string;
  Device: PAudioDevice;
begin
  EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
  begin
    LeaveCriticalSection(AudioCriticalSection);
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
    DebugLn('Pa_GetDeviceCount failed ');
    DebugLn('Error after Pa_GetDeviceCount ' + IntToStr(NumDevices));
  end;

  DebugLn('Enumerating devices:-');
  DebugLnEnter;
  for Count := 0 to NumDevices - 1 do
  begin
    DeviceInfo := Pa_GetDeviceInfo(Count);
    if DeviceInfo = nil then
    begin
      DebugLn('Error after GetDeviceInfo for device #' + IntToStr(Count));
    end
    else
    begin
      if DeviceInfo^.maxOutputChannels > 0 then
      begin
        DeviceName := StrPas(DeviceInfo^.Name);
        HostAPIInfo := Pa_GetHostApiInfo(DeviceInfo^.hostApi);

        if HostAPIInfo = nil then
        begin
          DebugLn('Error in getting HostAPI for devide #' +
            IntToStr(Count) + ' (' + DeviceName + ')');
        end;
        Device := New(PAudioDevice);

        Device^.DeviceName := Devicename;
        Device^.HostAPIName := HostAPIInfo^.Name;

        FDevices.Add(Device);
        DebugLn('Devide ID - ' + IntToStr(Count));
        DebugLn('Device name - ' + DeviceInfo^.Name);
        DebugLn('Host API name - ' + HostAPIInfo^.Name);
        DebugLn('Device output channels - ' + IntToStr(DeviceInfo^.maxOutputChannels));
      end;

    end;

  end;
  DebugLnExit;
  Result := FDevices;

  LeaveCriticalSection(AudioCriticalSection);
end;

class procedure TAudio.GetDefaultDevice(Device: PAudioDevice); static;
var
  DevideId: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
  DeviceName, HostAPIName: string;
begin
  EnterCriticalSection(AudioCriticalSection);
  DevideId := TAudio.GetDefaultDeviceIndex;
  DeviceInfo := Pa_GetDeviceInfo(DevideId);
  if DeviceInfo = nil then
  begin
    DebugLn('Error after GetDeviceInfo for device #' + IntToStr(DevideId));
    Exit;
  end;

  DeviceName := StrPas(DeviceInfo^.Name);
  HostAPIInfo := Pa_GetHostApiInfo(DeviceInfo^.hostApi);
  if HostAPIInfo = nil then
  begin
    DebugLn('Could not get HostAPI details');
    Exit;
  end;
  HostAPIName := StrPas(HostAPIInfo^.Name);

  Device^.DeviceName := DeviceName;
  Device^.HostAPIName := HostAPIName;

  LeaveCriticalSection(AudioCriticalSection);
end;

{procedure TAudio.SetFileName(AValue: string);
var
  TempSoundFile: PSNDFILE;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  if FAudioPlaying then
  begin
    DebugLn('Cannot call SetFileName when audio is playing');
    //LeaveCriticalSection(AudioCriticalSection);
    Exit;
  end;
  if AValue = '' then
  begin
    FFileName := '';
    FAudioLength := 0;
    if FSoundFile <> nil then
      sf_close(FSoundFile);
    Exit;
  end;


  TempSoundFile := sf_open(PChar(AValue), SFM_READ, @FInfo);
  if (TempSoundFile = nil) then
  begin
    DebugLn('Error in sf_open');
    //LeaveCriticalSection(AudioCriticalSection);
    raise EInvalidAudio.Create('sf_open returned nil for ' + AValue);
    Exit;
  end;

  if FSoundFile <> nil then
    sf_close(FSoundFile);

  FSoundFile := TempSoundFile;

  DebugLn(IntToHex(FInfo.format, 8));
  DebugLn(IntToStr(FInfo.channels));
  DebugLn(IntToStr(FInfo.frames));
  DebugLn(IntToStr(FInfo.samplerate));
  DebugLn(IntToStr(FInfo.sections));

  FFileName := AValue;
  FAudioLength := (FInfo.frames) / (FInfo.samplerate);
  //LeaveCriticalSection(AudioCriticalSection);
end;}

class procedure TAudio.SetOutputDevice(AValue: TAudioDevice);
begin
  try
    FOutputDevice.DeviceName := AValue.DeviceName;
    FOutputDevice.HostAPIName := AValue.HostAPIName;
  finally
  end;
end;



constructor TAudio.Create();
var
  i: integer;
begin

  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');
  //FSoundFile := nil;
  FAudioFile := nil;
  //FAudioFileLoaded := False;

  FSndAudioFile := TSndAudioFile.Create;
  FMpgAudioFile := TMpgAudioFile.Create;

  FDefAudioFile := TSndAudioFile.Create;
  FDefAudioFile.LoadDefaultSound;

  FTickAudioFile:=TSndAudioFile.Create;


  FAudioFile := FSndAudioFile;
  //FFileType := READ_NOTLOADED;


  FFileName := '';
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
  //DebugLn('TAudio.Destroy ');
  FTickAudioFile.Destroy;
  FDefAudioFile.Destroy;
  FMpgAudioFile.Destroy;
  FSndAudioFile.Destroy;
  inherited Destroy;
end;

class function TAudio.GetDefaultDeviceIndex: AudioDeviceIndex;
var
  DeviceId: AudioDeviceIndex;
begin
  EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  DeviceId := Pa_GetDefaultOutputDevice();
  if DeviceId = paNoDevice then
  begin
    DebugLn('No default device');
    LeaveCriticalSection(AudioCriticalSection);
    raise EInvalidDevice.Create('Pa_GetDefaultOutputDevice() returned PaNoDevice');
  end;
  Result := DeviceId;
  LeaveCriticalSection(AudioCriticalSection);
end;

class function TAudio.GetDeviceIndex(Device: TAudioDevice): AudioDeviceIndex;
var
  NumDevices, CountDevice: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
begin
  EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  NumDevices := Pa_GetDeviceCount();
  if NumDevices < 0 then
  begin
    DebugLn('Pa_GetDeviceCount failed ');
    DebugLn('Error after Pa_GetDeviceCount ' + IntToStr(NumDevices));
  end;

  for CountDevice := 0 to NumDevices - 1 do
  begin
    DeviceInfo := Pa_GetDeviceInfo(CountDevice);
    if DeviceInfo = nil then
    begin
      DebugLn('Error after GetDeviceInfo for device #' + IntToStr(CountDevice));
      Continue;
    end;

    if Device.DeviceName = StrPas(DeviceInfo^.Name) then
    begin
      HostAPIInfo := Pa_GetHostApiInfo(DeviceInfo^.hostApi);
      if HostAPIInfo = nil then
      begin
        DebugLn('Error after Pa_GetHostApiInfo for device #' +
          IntToStr(CountDevice) + ' host api #' + IntToStr(DeviceInfo^.hostApi));
        Continue;
      end;
      if HostAPIInfo^.Name = Device.HostAPIName then
      begin
        Result := CountDevice;
        LeaveCriticalSection(AudioCriticalSection);
        Exit;
      end;
    end;
  end;
  LeaveCriticalSection(AudioCriticalSection);
  raise EInvalidDevice.Create('No matching device for ' + Device.DeviceName +
    ':' + Device.HostAPIName);

end;

class procedure TAudio.CleanUp;
var
  Device: PAudioDevice;
begin
  if not TAudio.Loaded then
  begin
    DebugLn('TAudio.Loaded is fales in TAudio.Cleanup');
    Exit;
  end;

  for Device in TAudio.FDevices do
  begin
    Dispose(Device);
  end;
  TAudio.FDevices.Clear;

  FreeMem(TAudio.DefaultSound.Buffer);
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

procedure TAudio.Play;
{var
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;
  DeviceId: integer;}
begin
  //EnterCriticalSection(AudioCriticalSection);
  Play(FAudioFile, Looped);
  {try
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
      DebugLn('Sf_seek returned error');
    end;}
    FAudioFile.SeekToBeginning;

    if UseDefaultDevice or (FOutputDevice.HostAPIName = '') or
      (FOutputDevice.DeviceName = '') then
    begin
      DeviceId := DefaultDeviceIndex;
      DebugLn('TAudio using default device to play audio.');
    end
    else
    begin
      DeviceId := GetDeviceIndex(FOutputDevice);
      DebugLn('TAudio using device - ' + FOutputDevice.DeviceName +
        ' host api - ' + FOutputDevice.HostAPIName);
    end;
    StreamParams.device := DeviceId;

    DebugLn('Audio device is ' + IntToStr(StreamParams.device));

    //StreamParams.channelCount := FInfo.channels;
    StreamParams.channelCount:=FAudioFile.Channels;
    StreamParams.sampleFormat := FAudioFile.SampleFormat;

    Streamparams.suggestedLatency :=
      Pa_GetDeviceInfo(StreamParams.device)^.defaultLowOutputLatency;
    StreamParams.hostApiSpecificStreamInfo := nil;
    //DebugLn('Default device is ' + IntToStr(StreamParams.device));

    //FUserInfo.SoundFile := FSoundFile;

    FUserInfo.AudioFile:=nil;
    FUserInfo.Looped := Looped;
    FUserInfo.Player := Self;

    //Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

    PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams, FAudioFile.SampleRate,
      paFramesPerBufferUnspecified, paClipOff, PPaStreamCallback(@FeedAudioStream),
      @FUserInfo);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    PaErrCode := Pa_SetStreamFinishedCallback(FStream,
      PPaStreamFinishedCallback(@AudioStreamFinished));
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_SetStreamFinishedCallback failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_SetStreamFinishedCallback ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    PaErrCode := Pa_StartStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_StartStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_StartStream ' + IntToHex(PaErrCode, 8));
      Exit;
    end;

    FAudioPlaying := True;

  finally
    //LeaveCriticalSection(AudioCriticalSection);
  end; }

end;

procedure TAudio.PlayDefaultSound;
begin
  Play(FDefAudioFile, Looped);
end;

procedure TAudio.PlayTickSound;
begin
  Play(FTickAudioFile);
end;

procedure TAudio.PlayTest;
begin
  Play(FDefAudioFile, True);
end;

procedure TAudio.PlaySine;
var
  PaErrCode: PaError;
  OutputParameters: PaStreamParameters;
  DeviceId: integer;
begin
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');
  if UseDefaultDevice or (FOutputDevice.DeviceName = '') or
    (FOutputDevice.HostAPIName = '') then
  begin
    DeviceId := DefaultDeviceIndex;
    DebugLn('Playsine - using default device');
  end
  else
  begin
    DeviceId := GetDeviceIndex(FOutputDevice);
    DebugLn('TAudio.PlaySine using device - ' + FOutputDevice.DeviceName +
      ' host api - ' + FOutputDevice.HostAPIName);
  end;
  OutputParameters.Device := DeviceId;
  OutputParameters.ChannelCount := CInt32(2);
  OutputParameters.SampleFormat := paFloat32;
  OutputParameters.SuggestedLatency :=
    (Pa_GetDeviceInfo(OutputParameters.device)^.defaultHighOutputLatency) * 1;
  OutputParameters.HostApiSpecificStreamInfo := nil;

  DataPointer^.Player := Self;


  PaErrCode := Pa_OpenStream(@FStream, nil, @OutputParameters,
    SampleRate, FramesPerBuffer, paClipOff, PPaStreamCallback(@PaTestCallback),
    DataPointer);
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
  end;

  PaErrCode := Pa_SetStreamFinishedCallback(
    FStream, PPaStreamFinishedCallback(@StreamFinished));
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_SetStreamFinishedCallback failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_SetStreamFinishedCallback ' + IntToHex(PaErrCode, 8));
  end;

  PaErrCode := Pa_StartStream(FStream);

  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_StartStream failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_StartStream ' + IntToHex(PaErrCode, 8));
  end;

  FAudioPlaying := True;

end;

procedure TAudio.Abort;
var
  PaErrCode: PaError;
begin
  // TODO: Isn't this needed?
  //EnterCriticalSection(AudioCriticalSection);
  try
    if not TAudio.Loaded then
      raise EAudioNotLoaded.Create('Audio not loaded.');

    if not FAudioPlaying then
    begin
      DebugLn('Audio not playing. There is nothing to be done in abort');
      Exit;
    end;
    PaErrCode := Pa_AbortStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_AbortStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_AbortStream ' + IntToHex(PaErrCode, 8));
    end;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}

  finally
    //FAudioPlaying := False;
    //LeaveCriticalSection(AudioCriticalSection);
  end;

end;

{procedure TAudio.SetDefaultSound;
begin
  FSndAudioFile.LoadDefaultSound;

  FAudioFile := FSndAudioFile;
  //Duration := FAudioFile.Duration;
  FAudioFileLoaded:=true;
  FFileName:='';
end;}

procedure TAudio.FinishedAud(Datax: PtrInt);
var
  PaErrCode: PaError;
begin
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  EnterCriticalSection(AudioCriticalSection);

  {This check might be redundant. Just to be safe}
  Assert(FStream <> nil);
  paErrCode := Pa_IsStreamStopped(FStream);
  if paErrCode = 0 then
  begin
    paErrCode := Pa_StopStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_StopStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_StopStream ' + IntToHex(PaErrCode, 8));
    end;
  end
  else if PaErrCode <> 1 then
  begin
    DebugLn('Pa_IsStreamStopped failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_IsStreamStopped ' + IntToHex(PaErrCode, 8));
  end;
  paErrCode := Pa_CloseStream(FStream);
  if (paErrCode <> Int32(paNoError)) then
  begin
    DebugLn('Pa_CloseStream failed ' + Pa_GetErrorText(paErrCode));
    DebugLn('Error after Pa_CloseStream ' + IntToHex(PaErrCode, 8));
  end;
  FStream := nil;
  FAudioPlaying := False;
  if OnPlayCompletion <> nil then
    OnPlayCompletion(Self);
  LeaveCriticalSection(AudioCriticalSection);
end;

procedure TAudio.UnloadAudioFile;
begin
  FAudioFile:=nil;
  //FAudioFileLoaded:=false;
  FFileName := '';
end;

procedure TAudio.LoadFromFile(AValue: string);
{var
  FSnd: TSndAudioFile = nil;}
begin
  //FAudioFileLoaded:=false;
  if AValue <> '' then
  begin
    {try
      FSndAudioFile.SetFileName(AValue);
      FAudioFile := FSndAudioFile;
    except
      on E: EInvalidAudio do
      try
        FMpgAudioFile.SetFileName(AValue);
        FAudioFile := FMpgAudioFile;
      except
        on E: EInvalidAudio do
          raise EInvalidAudio.Create('sndfile & mpg123 returned error for ' + AValue);
      end;
    end; }
    FSndAudioFile.SetFileName(AValue);
    if FSndAudioFile.FileName <> '' then
    begin
      Assert(FSndAudioFile <> nil);
      FAudioFile := FSndAudioFile;
      //Duration := FAudioFile.Duration;
      //FAudioFileLoaded:=true;
      FFileName:=AValue;
      Exit;
    end;

    FMpgAudioFile.SetFileName(AValue);
    if FMpgAudioFile.FileName <> '' then
    begin
      Assert(FMpgAudioFile <> nil);
      FAudioFile := FMpgAudioFile;
      //FAudioFileLoaded:=true;
      FFileName:=AValue;
      Exit;
    end;

    raise EInvalidAudio.Create('sndfile & mpg123 returned error for ' + AValue);

  end;

end;

var
  PaErrCode: PaError;


initialization
  TAudio.Loaded := False;
  TAudio.DefaultSound.Loaded:=False;
  TAudio.FDevices := TAudioDeviceList.Create;

  {$IFNDEF AUDIO_STATIC}
  TAudio.Loaded := Pa_Load(LIB_PORTAUDIO);
  if not TAudio.Loaded then
  begin
    DebugLn('Could not load portaudio');
    Exit;
  end;

  { Load sndfile library only if portaudio was loaded successfully }

  if TAudio.Loaded then
  begin
    TAudio.Loaded := sf_load(LIB_SNDFILE);
    if not TAudio.Loaded then
    begin
      DebugLn('Could not load sndfile');
      Pa_Unload();
      Exit;
    end;
  end;

  if TAudio.Loaded then
  begin
    TAudio.Loaded := Mp_Load(LIB_MPG123);
    if not TAudio.Loaded then
    begin
      DebugLn('Could not load mpg123');
      sf_Unload;
      Pa_Unload;
      Exit;
    end;
   if mpg123_init() <> MPG123_OK then
   begin
     TAudio.Loaded := False;
     DebugLn('mpg123_init() failed');
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
      DebugLn('Error in Pa_Initialize()');

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
      DebugLn('No default device');
      TAudio.Loaded := False;
      Pa_Terminate;
      {$IFNDEF AUDIO_STATIC}
      Mp_Unload;
      sf_Unload;
      Pa_Unload;
      {$ENDIF}
    end;
  end;
  InitCriticalSection(TAudio.AudioCriticalSection);


finalization
  DoneCriticalsection(TAudio.AudioCriticalSection);
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
