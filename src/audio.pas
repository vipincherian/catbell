unit audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sndfile, mpg123, portaudio, {EventLog,} ctypes, Forms,
  Dialogs, LCLIntf, lcltype, fgl, Math, log, sound;

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

  TRawSoundData = record
    Buffer: PAnsiChar;
    Size: integer;
    ChannelCount: integer;
    SampleFormat: PaSampleFormat;
    //Loaded: boolean;
  end;
  PRawSoundData = ^TRawSoundData;
  //TSeekableRawSoundData = TSeekableSoundData;
  //PSeekableRawSoundData = PSeekableSoundData;

  TSeekableRawSoundData = record
    RawSound: PRawSoundData;
    //Channels: integer;
    Position: sf_count_t;
  end;
  PSeekableRawSoundData = ^TSeekableRawSoundData;

  TAudioDevice = record
    HostAPIName: string;
    DeviceName: string;
  end;

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

  PAudioDevice = ^TAudioDevice;
  TAudioDeviceList = specialize TFPGList<PAudioDevice>;

  { The record passed to port audio callbacks }
  TUserInfo = record
    Sound: TSound;
    Player: Pointer;
    Looped: boolean;
    Volume: ^integer;
  end;
  PAudioInfo = ^TUserInfo;

  TRawUserInfo = record
    RawSeekable: TSeekableRawSoundData;
    Player: Pointer;
  end;
  PRawUserInfo = ^TRawUserInfo;

  //PSoundData = ^TSoundData;

  { TAudioPlayer }
  TAudioPlayer = class(TObject)
  private

    FUserInfo: TUserInfo;
    FRawUserInfo: TRawUserInfo;
    FStream: PPaStream;
    FSeekableSoundData: TSeekableRawSoundData;

    FAudioPlaying: boolean;

    Data: PaTestData;
    DataPointer: PPaTestData;

    //procedure SetVolume(AValue: integer);

  public


    OnPlayCompletion: TNotifyEvent;
    Looped: boolean;
    constructor Create;
    destructor Destroy; override;

    procedure Play(Sound: TSound; var Volume: integer; PlayLooped: boolean = False);
    procedure Play(RawSound: PRawSoundData);
    procedure Abort;
    procedure FinishedAud({%H-}Datax: PtrInt);
    property Playing: boolean read FAudioPlaying;

  end;

  { TAudioSystem }
  TAudioSystem = class(TObject)
  private
    FLoaded: boolean;
    FOutputDevice: TAudioDevice;
    {Default sounds }
    FDefaultSound: TSoundData;
    FDefaultTick: TSoundData;
    FDevices: TAudioDeviceList;
    FDefaultDevice: integer;
    {%H-}constructor Create;
    {%H-}destructor {%H-}Destroy; override;


    function GetDevices: TAudioDeviceList;
    procedure SetOutputDevice(AValue: TAudioDevice);
    procedure CleanUp;
  public

    UseDefaultDevice: boolean;
    //AudioCriticalSection: TRTLCriticalSection; static;
    //FVolume: integer;
    function GetDefaultDeviceIndex: AudioDeviceIndex;
    procedure GetDefaultDevice(Device: PAudioDevice);
    function GetDeviceIndex(Device: TAudioDevice): AudioDeviceIndex;
    property DefaultDeviceIndex: AudioDeviceIndex read GetDefaultDeviceIndex;
    property Devices: TAudioDeviceList read GetDevices;
    //property Volume: integer read FVolume write SetVolume;

    procedure SetDefaulDevice;
    procedure LoadDefaultSounds;
    procedure FreeDefaultSounds;
    property OutputDevice: TAudioDevice read FOutputDevice write SetOutputDevice;
    property Loaded: boolean read FLoaded;
    function LoadSound(Avalue: string): TSound;
    property DefaultSound: TSoundData read FDefaultSound;
    property DefaultTick: TSoundData read FDefaultTick;
  end;

  TSoundPoolEntry = record
    Original: PSoundData;
    Raw: PRawSoundData;
    //RawVolumeAdjusted: PRawSoundData;
  end;
  PSoundPoolEntry = ^TSoundPoolEntry;

  TSoundPoolList = specialize TFPGList<PSoundPoolEntry>;



  { TSoundPool }
  TSoundPool = class(TObject)
  private
    FEntries: TSoundPoolList;
    FDefaultSoundIndex, FTickIndex: integer;
    {%H-}constructor Create;
    {%H-}destructor {%H-}Destroy; override;
    function GetRawDefaultSound: PRawSoundData;
    procedure RefillRawSound(const Index: integer);
    function LoadDefaultSound(const ResourceID: string): integer;
  public
    procedure LoadAllDefaultSounds;
    procedure LoadSoundFromResource(ResourceName: string; var Sound: TSoundData);
    property RawDefaultSound: PRawSoundData read GetRawDefaultSound;

  end;

var
  PaErrCode: PaError;
  AudioSystem: TAudioSystem = nil;
  SoundPool: TSoundPool = nil;

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
  Volume: integer;
  AmpScale: double;
begin
  //EnterCriticalSection(TAudioPlayer.AudioCriticalSection);
  //Logger.Debug('Inside FeedAudioStream');

  AudioInfo := PAudioinfo(userData);

  Assert(AudioInfo^.Sound <> nil);

  UsedSound := AudioInfo^.Sound;

  readSuccess := False;
  readSuccess := (UsedSound.Read(output, frameCount) > 0);

  // Decode and fix volume
  Volume := AudioInfo^.Volume^;

  Assert((Volume >= 0) and (Volume <= MAX_VOLUME));
  AmpScale := Volume / MAX_VOLUME;
  AmpScale := (power(VOLUME_LOG_BASE, AmpScale) - 1) / (VOLUME_LOG_BASE - 1);
  Assert((AmpScale >= 0) and (AmpScale <= 1));

  { Apply scaling to amplitude to control volume }
  Data := output;
  for Count := 0 to (AudioInfo^.Sound.Channels * frameCount) - 1 do
  begin
    Data[Count] := Data[Count] * AmpScale; //AudioInfo^.Volume;
  end;


  if AudioInfo^.Looped then
  begin
    { If audio is lopped and if no audio data could be read,
    seek to the beginning and then read again }
    if not readSuccess then
    begin

      UsedSound.SeekToBeginning;

      readSuccess := False;
      readSuccess := (UsedSound.Read(output, frameCount) > 0);

      { Apply scaling to amplitude to control volume }
      Data := output;
      for Count := 0 to (AudioInfo^.Sound.Channels * frameCount) - 1 do
      begin
        Data[Count] := Data[Count] * AmpScale; //AudioInfo^.Volume;
      end;

      if not readSuccess then
      begin
        //Logger.Debug('readCount zero immediately after seek to beginning');
        Result := cint(paAbort);
        //LeaveCriticalSection(TAudioPlayer.AudioCriticalSection);
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

  //LeaveCriticalSection(TAudioPlayer.AudioCriticalSection);
end;

function FeedAudioStreamBuffered({%H-}input: pointer; output: pointer;
  frameCount: culong;
  {%H-}timeInfo: PPaStreamCallbackTimeInfo; {%H-}statusFlags: PaStreamCallbackFlags;
  userData: pointer): cint; cdecl;
var
  AudioInfo: PRawUserInfo;
  BytesRead: integer;
  //readSuccess: boolean;
  //UsedSound: TSound;
  //Data: pcfloat;
  //Count: integer;
  //Volume: integer;
  //AmpScale: double;
begin
  //EnterCriticalSection(TAudioPlayer.AudioCriticalSection);
  //Logger.Debug('Inside FeedAudioStream');

  { Log statements in this callback will invariably result in a crash }

  AudioInfo := PRawUserinfo(userData);

  //Assert(AudioInfo <> nil);
  //Assert(AudioInfo^.RawSeekable.Position <= AudioInfo^.RawSeekable.RawSound^.Size);
  //Assert(AudioInfo^.Sound^. <> nil);
  //AudioInfo^.Sound.;

  BytesRead := Min(AudioInfo^.RawSeekable.RawSound^.Size -
    AudioInfo^.RawSeekable.Position + 1, frameCount *
    { TODO : Remove the hardcoding of channel count }2 * sizeof(cfloat));

  //Logger.Debug('frameCount - ' + IntToStr(frameCount));
  //Logger.Debug('BytesRead - ' + IntToStr(BytesRead));
  //Logger.Debug('AudioInfo^.RawSeekable.Position - ' + IntToStr(AudioInfo^.RawSeekable.Position));
  //Logger.Debug('AudioInfo^.RawSeekable.Sound^.Size - ' + IntToStr(AudioInfo^.RawSeekable.RawSound^.Size));
  //Logger.Debug('Soundpool - AudioInfo^.RawSeekable.Sound^.Buffer - '
  //    + IntToHex(PQWord(AudioInfo^.RawSeekable.RawSound^.Buffer)[0], 16)
  //    + ' ' + IntToHex(PQWord(AudioInfo^.RawSeekable.RawSound^.Buffer)[1], 16)
  //    );
  Move((AudioInfo^.RawSeekable.RawSound^.Buffer +
    AudioInfo^.RawSeekable.Position)^, output^,
    BytesRead);
  //Logger.Debug('Soundpool - AudioInfo^.RawSeekable.Sound^.Buffer - '
  //    + IntToHex(PQWord(AudioInfo^.RawSeekable.RawSound^.Buffer)[0], 16)
  //    + ' ' + IntToHex(PQWord(AudioInfo^.RawSeekable.RawSound^.Buffer)[1], 16)
  //    );
  AudioInfo^.RawSeekable.Position += BytesRead;

  if (AudioInfo^.RawSeekable.Position <= AudioInfo^.RawSeekable.RawSound^.Size) then
  begin
    Result := cint(paContinue);
  end
  else
  begin
    AudioInfo^.RawSeekable.Position := 0;
    Result := cint(paComplete);
  end;

end;

{This function is called by PortAudio to signal that the stream has stopped.
As this is a non-class function, it sends a message to frame using the frame's
handle.}
procedure AudioStreamFinished(UserData: pointer); cdecl;
var
  AudioInfo: PAudioInfo;
  {%H-}AudioTemp: TAudioPlayer; // Possibly buggy hint, omitting
begin
  //EnterCriticalSection(TAudioPlayer.AudioCriticalSection);

  AudioInfo := PAudioinfo(userData);

  AudioTemp := TAudioPlayer(AudioInfo^.Player);

  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  //LeaveCriticalSection(TAudioPlayer.AudioCriticalSection);
end;

procedure AudioStreamFinishedRaw(UserData: pointer); cdecl;
var
  AudioInfo: PRawUserInfo;
  {%H-}AudioTemp: TAudioPlayer; // Possibly buggy hint, omitting
begin
  //EnterCriticalSection(TAudioPlayer.AudioCriticalSection);

  AudioInfo := PRawUserInfo(userData);

  AudioTemp := TAudioPlayer(AudioInfo^.Player);

  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  //LeaveCriticalSection(TAudioPlayer.AudioCriticalSection);
end;

{ TSoundPool }

constructor TSoundPool.Create;
begin
  FEntries := TSoundPoolList.Create;
  //LoadAllDefaultSounds;

end;

destructor TSoundPool.Destroy;
var
  SoundPoolEntry: PSoundPoolEntry;
begin
  for SoundPoolEntry in FEntries do
  begin
    FreeMem(SoundPoolEntry^.Original^.Buffer);
    Dispose(SoundPoolEntry^.Original);
    FreeMem(SoundPoolEntry^.Raw^.Buffer);
    Dispose(SoundPoolEntry^.Raw);
    //FreeMem(SoundPoolEntry^.RawVolumeAdjusted^.Buffer);
    //Dispose(SoundPoolEntry^.RawVolumeAdjusted);
    Dispose(SoundPoolEntry);
  end;
  FEntries.Free;
  inherited Destroy;
end;

function TSoundPool.GetRawDefaultSound: PRawSoundData;
var
  //Buffer: PSeekableRawSoundData;
  SoundPoolEntry: PSoundPoolEntry;
begin
  { TODO : Change to a single line. }
  SoundPoolEntry := FEntries.Items[FDefaultSoundIndex];
  Result := SoundPoolEntry^.Raw;
end;

procedure TSoundPool.RefillRawSound(const Index: integer);
var
  SoundPoolEntry: PSoundPoolEntry;
  SndFile: TSndSound;
  Size: integer;
  Read: integer;
  //VolumeBuffer: PCFloat;
  RawBuffer: PCFloat;
  Count: integer;
  Volume: integer;
  AmpScale: double;
begin
  SoundPoolEntry := FEntries.Items[Index];

  SndFile := TSndSound.Create;
  SndFile.LoadInMemorySound(SoundPoolEntry^.Original);

  Logger.Debug('Soundpool - loading default sound');
  Logger.Debug('Soundpool - SndFile.Channels - ' + IntToStr(SndFile.Channels));
  Logger.Debug('Soundpool - SndFile.FrameLength - ' + IntToStr(SndFile.FrameLength));
  Logger.Debug('Soundpool - SizeOf(cfloat) - ' + IntToStr(SizeOf(cfloat)));
  Logger.Debug('Soundpool - SoundPoolEntry^.Original^.Size - ' +
    IntToStr(SoundPoolEntry^.Original^.Size));

  { Create the record to hold raw sound }


  if SoundPoolEntry^.Raw <> nil then
  begin
    FreeMem(SoundPoolEntry^.Raw^.Buffer);
    Dispose(SoundPoolEntry^.Raw);
  end;

  SoundPoolEntry^.Raw := New(PRawSoundData);
  SoundPoolEntry^.Raw^.Buffer :=
    AllocMem(SndFile.FrameLength * SndFile.Channels * SizeOf(cfloat));
  SoundPoolEntry^.Raw^.Size :=
    (SndFile.FrameLength * SndFile.Channels * SizeOf(cfloat));

  { Create the record to hold raw volume adjusted sound }
  //SoundPoolEntry^.RawVolumeAdjusted := New(PRawSoundData);
  //SoundPoolEntry^.RawVolumeAdjusted^.Buffer := AllocMem(SoundPoolEntry^.Raw^.Size);
  //SoundPoolEntry^.RawVolumeAdjusted^.Size := SoundPoolEntry^.Raw^.Size;
  //Logger.Debug('Soundpool - SoundPoolEntry^.RawVolumeAdjusted^.Size - ' + IntTostr(SoundPoolEntry^.RawVolumeAdjusted^.Size));

  //SoundPoolEntry^.Original^
  { Read raw data and keep it raedy for use }
  Size := 0;
  Read := 0;
  repeat
    begin
        { SndFile.Read returns the number of floats read. This already takes the
        number of channels to account, so no need to multiply }
      Read := SndFile.Read(SoundPoolEntry^.Raw^.Buffer +
        (Size * SizeOf(cfloat)), 10000);

      //Read := SndFile.Read(@Buffer[0], 10);
      Logger.Debug('Read bytes - ' + IntToStr(Read));
      Size += Read;
    end;
  until (Size >= SndFile.FrameLength * SndFile.Channels);

  SoundPoolEntry^.Raw^.ChannelCount := SndFile.Channels;
  SoundPoolEntry^.Raw^.SampleFormat := SndFile.SampleFormat;

  Logger.Debug('Soundpool - SoundPoolEntry^.RawVolumeAdjusted^.Buffer - ' +
    IntToHex(PQWord(SoundPoolEntry^.Raw^.Buffer)[0], 16) + ' ' +
    IntToHex(PQWord(SoundPoolEntry^.Raw^.Buffer)[1], 16)
    );
  { Fill the volume adjusted raw data too. This is used for playing }

  RawBuffer := PCFloat(SoundPoolEntry^.Raw^.Buffer);
  //VolumeBuffer := PCFloat(SoundPoolEntry^.RawVolumeAdjusted^.Buffer);

  Volume := GlobalUserConfig.Volume;
  Assert((Volume >= 0) and (Volume <= MAX_VOLUME));
  AmpScale := Volume / MAX_VOLUME;
  AmpScale := (power(VOLUME_LOG_BASE, AmpScale) - 1) / (VOLUME_LOG_BASE - 1);
  Assert((AmpScale >= 0) and (AmpScale <= 1));

  { Apply scaling to amplitude to control volume }
  for Count := 0 to (SndFile.FrameLength * SndFile.Channels) - 1 do
  begin
    RawBuffer[Count] := RawBuffer[Count] * AmpScale; //AudioInfo^.Volume;
  end;

  //FreeMem(Buffer);
  Logger.Debug('Size of default sound - ' + IntToStr(Size));
  SndFile.Free;
end;

function TSoundPool.LoadDefaultSound(const ResourceID: string): integer;
var
  SoundPoolEntry: PSoundPoolEntry;
  //SndFile: TSndSound;
  //Size: integer;
  //Read: integer;
  //VolumeBuffer: PCFloat;
  //RawBuffer: PCFloat;
  //Count: integer;
  //Volume: integer;
  //AmpScale: double;
  Index: integer = -1;
begin

  try
    SoundPoolEntry := New(PSoundPoolEntry);

    { Create the record to hold file contents }
    SoundPoolEntry^.Original := New(PSoundData);
    LoadSoundFromResource(ReSourceID, SoundPoolEntry^.Original^);

    SoundPoolEntry^.Raw := nil;
    Index := FEntries.Add(SoundPoolEntry);
    Result := Index;

    RefillRawSound(Index);

  except
    on E: Exception do
    begin
      Logger.Error('Could not fill raw sound for resource ID - ' + ResourceID);
      Logger.Error(E.Message);
      Result := -1;
      Exit;
    end;
  end;


end;

procedure TSoundPool.LoadAllDefaultSounds;
var
  ResourceID: string;
begin
  if AudioSystem.Loaded then
  begin
    FDefaultSoundIndex := LoadDefaultSound('DEFAULT_SOUND');
    FTickIndex := LoadDefaultSound('TICK');
    if (FDefaultSoundIndex < 0) or (FTickIndex < 0) then
    begin
      Logger.Error('Could not load one of the default sounds');
      ShowMessage('Fatal Error - Could not load one of the default sounds');
      Application.Terminate;
    end;
  end;
end;

procedure TSoundPool.LoadSoundFromResource(ResourceName: string; var Sound: TSoundData);
var
  BytesRead: integer;
  Size: integer;
  Stream: TResourceStream;
begin
  Logger.Debug('Loading sound from resource - ' + ResourceName);
  Stream := TResourceStream.Create(hinstance, ResourceName, RT_RCDATA);
  Size := Stream.Size;
  Logger.Debug('Stream.Size - ' + IntToStr(Stream.Size));
  Assert(Size > 0);
  if Size <= 0 then
    Logger.Warning('Stream.Size is ' + IntToStr(Size) + ' at TAudio.LoadDefaultSounds'
      );

  Sound.Buffer := AllocMem(Size);
  Sound.Size := Size;
  BytesRead := 0;
  BytesRead := Stream.Read(Sound.Buffer^, Size);

  if BytesRead <> Size then
    Logger.Warning('BytesRead does not match Size in TAudio.LoadDefaultSounds');

  //Logger.Info('Size of the stream is ' + IntToStr(Size));
  //Sound.Loaded := True;
  Stream.Free;
end;

{ This is called when playback is finished.
  Remember: ALWAYS USE CDECL or your pointers will be messed up!
  Pointers to this function must be castable to PPaStreamFinishedCallback: }
{procedure StreamFinished(UserData: pointer); cdecl;
var
  LocalDataPointer: PPaTestData;
  {%H-}AudioTemp: TAudioPlayer; // buggy hint, omitting
begin
  EnterCriticalSection(TAudioPlayer.AudioCriticalSection);
  LocalDataPointer := PPaTestData(UserData);
  AudioTemp := TAudioPlayer(LocalDataPointer^.Player);
  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  LeaveCriticalSection(TAudioPlayer.AudioCriticalSection);
end;}


{ TAudioSystem }

constructor TAudioSystem.Create;
begin
  FLoaded := False;
  //FDefaultSound.Loaded := False;
  //FDefaultTick.Loaded := False;
  FDevices := TAudioDeviceList.Create;

  {$IFNDEF AUDIO_STATIC}
  FLoaded := Pa_Load(LIB_PORTAUDIO);
  if not FLoaded then
  begin
    //Logger.Debug('Could not load portaudio');
    Exit;
  end;

  { Load sndfile library only if portaudio was loaded successfully }

  if FLoaded then
  begin
    FLoaded := sf_load(LIB_SNDFILE);
    if not FLoaded then
    begin
      //Logger.Debug('Could not load sndfile');
      Pa_Unload();
      Exit;
    end;
  end;

  if FLoaded then
  begin
    FLoaded := Mp_Load(LIB_MPG123);
    if not FLoaded then
    begin
      //Logger.Debug('Could not load mpg123');
      sf_Unload;
      Pa_Unload;
      Exit;
    end;
    if mpg123_init() <> MPG123_OK then
    begin
      FLoaded := False;
      //Logger.Debug('mpg123_init() failed');
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

  if FLoaded then
  begin
    PaErrCode := Pa_Initialize();
    if PaErrCode <> cint(paNoError) then
    begin
      Logger.Debug('Error in Pa_Initialize()');

      FLoaded := False;

      { If portaudio cannot be initialised, then audio will not work.
      Unload libraries }
      {$IFNDEF AUDIO_STATIC}
      Mp_Unload;
      sf_Unload;
      Pa_Unload;

      {$ENDIF}
    end;
  end;

  if FLoaded then
  begin
    FDefaultDevice := Pa_GetDefaultOutputDevice();
    if FDefaultDevice = paNoDevice then
    begin
      Logger.Debug('No default device');
      FLoaded := False;
      Pa_Terminate;
      {$IFNDEF AUDIO_STATIC}
      Mp_Unload;
      sf_Unload;
      Pa_Unload;
      {$ENDIF}
    end;
  end;
  //InitCriticalSection(TAudioPlayer.AudioCriticalSection);
end;

destructor TAudioSystem.Destroy;
begin
  if FLoaded then
  begin
    Pa_Terminate;
    mpg123_exit;
    {$IFNDEF AUDIO_STATIC}
    Mp_Unload;
    Sf_Unload;
    Pa_Unload;
    {$ENDIF}
  end;
  CleanUp;
  FDevices.Free;

  inherited Destroy;
end;



{ TAudioPlayer }

procedure TAudioPlayer.Play(Sound: TSound; var Volume: integer; PlayLooped: boolean);
var
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;
  DeviceId: integer;
  //AmpScale: double;
begin
  //EnterCriticalSection(AudioCriticalSection);

  try
    if not AudioSystem.Loaded then
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

    if AudioSystem.UseDefaultDevice or (AudioSystem.OutputDevice.HostAPIName = '') or
      (AudioSystem.OutputDevice.DeviceName = '') then
    begin
      DeviceId := AudioSystem.DefaultDeviceIndex;
      Logger.Info('TAudio using default device to play audio.');
    end
    else
    begin
      DeviceId := AudioSystem.GetDeviceIndex(AudioSystem.OutputDevice);
      Logger.Info('TAudio using device - ' + AudioSystem.OutputDevice.DeviceName +
        ' host api - ' + AudioSystem.OutputDevice.HostAPIName);
    end;
    StreamParams.device := DeviceId;

    Logger.Debug('Audio device is ' + IntToStr(StreamParams.device));

    //StreamParams.channelCount := FInfo.channels;
    StreamParams.channelCount := Sound.Channels;
    StreamParams.sampleFormat := Sound.SampleFormat;

    Streamparams.suggestedLatency :=
      //0.5;
      Pa_GetDeviceInfo(StreamParams.device)^.defaultLowOutputLatency;
    StreamParams.hostApiSpecificStreamInfo := nil;
    //Logger.Debug('Default device is ' + IntToStr(StreamParams.device));

    //FUserInfo.SoundFile := FSoundFile;

    FUserInfo.Sound := Sound;
    FUserInfo.Looped := PlayLooped;
    FUserInfo.Player := Self;

    //Assert((Volume >= 0) and (Volume <= MAX_VOLUME));
    //AmpScale := Volume / MAX_VOLUME;
    //FUserInfo.Volume := (power(VOLUME_LOG_BASE, AmpScale) - 1) / (VOLUME_LOG_BASE - 1);
    //Assert((FUserInfo.Volume >= 0) and (FUserInfo.Volume <= 1));
    FUserInfo.Volume := @Volume;

    //Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

    PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams,
      Sound.SampleRate, paFramesPerBufferUnspecified,
      //2024,
      paClipOff, PPaStreamCallback(@FeedAudioStream), @FUserInfo);
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

procedure TAudioPlayer.Play(RawSound: PRawSoundData);
var
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;
  DeviceId: integer;
begin
  if not AudioSystem.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  if FAudioPlaying then
  begin
    { TODO : Handle this situation more elegantly }
    ShowMessage('Again?');
    //LeaveCriticalsection(AudioCriticalSection);
    Exit;
  end;

  {if sf_seek(FSoundFile, 0, SEEK_SET) = -1 then
  begin
    Logger.Debug('Sf_seek returned error');
  end;}
  FSeekableSoundData.Position := 0;
  FSeekableSoundData.RawSound := RawSound;


  if AudioSystem.UseDefaultDevice or (AudioSystem.OutputDevice.HostAPIName = '') or
    (AudioSystem.OutputDevice.DeviceName = '') then
  begin
    DeviceId := AudioSystem.DefaultDeviceIndex;
    Logger.Info('TAudio using default device to play audio.');
  end
  else
  begin
    DeviceId := AudioSystem.GetDeviceIndex(AudioSystem.OutputDevice);
    Logger.Info('TAudio using device - ' + AudioSystem.OutputDevice.DeviceName +
      ' host api - ' + AudioSystem.OutputDevice.HostAPIName);
  end;
  StreamParams.device := DeviceId;

  Logger.Debug('Audio device is ' + IntToStr(StreamParams.device));

  //StreamParams.channelCount := FInfo.channels;
  StreamParams.channelCount := RawSound^.ChannelCount;
  StreamParams.sampleFormat := RawSound^.SampleFormat;

  Streamparams.suggestedLatency :=
    (Pa_GetDeviceInfo(StreamParams.device)^.defaultHighOutputLatency);
  StreamParams.hostApiSpecificStreamInfo := nil;
  //Logger.Debug('Default device is ' + IntToStr(StreamParams.device));

  //FUserInfo.SoundFile := FSoundFile;

  FRawUserInfo.RawSeekable := FSeekableSoundData;
  //FUserInfo.Looped := PlayLooped;
  FRawUserInfo.Player := Self;

  //Assert((Volume >= 0) and (Volume <= MAX_VOLUME));
  //AmpScale := Volume / MAX_VOLUME;
  //FUserInfo.Volume := (power(VOLUME_LOG_BASE, AmpScale) - 1) / (VOLUME_LOG_BASE - 1);
  //Assert((FUserInfo.Volume >= 0) and (FUserInfo.Volume <= 1));
  //FUserInfo.Volume := @Volume;

  //Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

  Logger.Debug('');
  Logger.Debug('Before Play');
  Logger.Debug('');
  Logger.Debug('Play - FSeekableSoundData.Sound^.Buffer - ' +
    IntToStr(FSeekableSoundData.RawSound^.Size));
  Logger.Debug('Play - FSeekableSoundData.Sound^.Buffer - ' +
    IntToHex(PQWord(FSeekableSoundData.RawSound^.Buffer)[0], 16) +
    ' ' + IntToHex(PQWord(FSeekableSoundData.RawSound^.Buffer)[1], 16)
    );


  PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams, 44100,
    paFramesPerBufferUnspecified, paClipOff or paDitherOff,
    PPaStreamCallback(@FeedAudioStreamBuffered), @FRawUserInfo);
  if (paErrCode <> Int32(paNoError)) then
  begin
    Logger.Debug('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
    Logger.Debug('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
    Exit;
  end;

  PaErrCode := Pa_SetStreamFinishedCallback(FStream,
    PPaStreamFinishedCallback(@AudioStreamFinishedRaw));
  if (paErrCode <> Int32(paNoError)) then
  begin
    Logger.Debug('Pa_SetStreamFinishedCallback failed ' + Pa_GetErrorText(paErrCode));
    Logger.Debug('Error after Pa_SetStreamFinishedCallback ' + IntToHex(PaErrCode, 8));
    Exit;
  end;

  Logger.Debug('Play - pa_GetStreamInfo(FStream)^.outputLatency - ' +
    FloatToStr(pa_GetStreamInfo(FStream)^.outputLatency));

  PaErrCode := Pa_StartStream(FStream);
  if (paErrCode <> Int32(paNoError)) then
  begin
    Logger.Debug('Pa_StartStream failed ' + Pa_GetErrorText(paErrCode));
    Logger.Debug('Error after Pa_StartStream ' + IntToHex(PaErrCode, 8));
    Exit;
  end;

  Logger.Debug('Play - pa_GetStreamInfo(FStream)^.outputLatency - ' +
    FloatToStr(pa_GetStreamInfo(FStream)^.outputLatency));

  FAudioPlaying := True;
end;


function TAudioSystem.GetDevices: TAudioDeviceList;
var
  NumDevices, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
  DeviceName: string;
  Device: PAudioDevice;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not AudioSystem.Loaded then
  begin
    //LeaveCriticalSection(AudioCriticalSection);
    raise EAudioNotLoaded.Create('Audio not loaded.');
  end;

  for Device in FDevices do
  begin
    Dispose(Device);
  end;
  FDevices.Clear;
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

procedure TAudioSystem.GetDefaultDevice(Device: PAudioDevice);
var
  DevideId: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
  DeviceName, HostAPIName: string;
begin
  //EnterCriticalSection(AudioCriticalSection);
  DevideId := GetDefaultDeviceIndex;
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

procedure TAudioSystem.SetOutputDevice(AValue: TAudioDevice);
begin
  try
    FOutputDevice.DeviceName := AValue.DeviceName;
    FOutputDevice.HostAPIName := AValue.HostAPIName;
  finally
  end;
end;

{procedure TAudioPlayer.SetVolume(AValue: integer);
begin
  if FVolume=AValue then Exit;
  FVolume:=Min(AValue, MAX_VOLUME);
end;}

constructor TAudioPlayer.Create;
var
  i: integer;
begin

  if not AudioSystem.Loaded then
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

destructor TAudioPlayer.Destroy;
begin
  inherited Destroy;
end;

function TAudioSystem.GetDefaultDeviceIndex: AudioDeviceIndex;
var
  DeviceId: AudioDeviceIndex;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not FLoaded then
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

function TAudioSystem.GetDeviceIndex(Device: TAudioDevice): AudioDeviceIndex;
var
  NumDevices, CountDevice: integer;
  DeviceInfo: PPaDeviceInfo;
  HostAPIInfo: PPaHostApiInfo;
begin
  //EnterCriticalSection(AudioCriticalSection);
  if not FLoaded then
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

procedure TAudioSystem.CleanUp;
var
  Device: PAudioDevice;
begin
  if not FLoaded then
  begin
    Logger.Debug('TAudio.Loaded is fales in TAudio.Cleanup');
    Exit;
  end;

  for Device in FDevices do
  begin
    Dispose(Device);
  end;
  FDevices.Clear;

  FreeMem(FDefaultSound.Buffer);
  FreeMem(FDefaultTick.Buffer);
end;

{ TODO : Is this implementation ?}
procedure TAudioSystem.SetDefaulDevice;
begin
  GetDefaultDevice(@FOutputDevice);
end;

procedure TAudioSystem.LoadDefaultSounds;
begin
  if not FLoaded then
  begin
    Exit;
  end;
  SoundPool.LoadSoundFromResource('DEFAULT_SOUND', FDefaultSound);
  SoundPool.LoadSoundFromResource('TICK', FDefaultTick);
end;

procedure TAudioSystem.FreeDefaultSounds;
begin
  FreeMem(FDefaultSound.Buffer);
  FreeMem(FDefaultTick.Buffer);
end;

procedure TAudioPlayer.Abort;
var
  PaErrCode: PaError;
begin
  //EnterCriticalSection(AudioCriticalSection);
  try
    if not AudioSystem.Loaded then
      raise EAudioNotLoaded.Create('Audio not loaded.');

    if not FAudioPlaying then
    begin
      Logger.Debug('Audio not playing. There is nothing to be done in abort');
      Exit;
    end;
    {Pa_AbortStream throws an exception in Linux, using  }
    PaErrCode := Pa_StopStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      Logger.Debug('Pa_StopStream failed ' + Pa_GetErrorText(paErrCode));
      Logger.Debug('Error after Pa_StopStream ' + IntToHex(PaErrCode, 8));
    end;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}

  finally
    //FAudioPlaying := False;
    //LeaveCriticalSection(AudioCriticalSection);
  end;

end;

procedure TAudioPlayer.FinishedAud(Datax: PtrInt);
var
  PaErrCode: PaError;
begin
  if not AudioSystem.Loaded then
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

function TAudioSystem.LoadSound(Avalue: string): TSound;
var
  SndSound: TSndSound;
  MpgSound: TMpgSound;
begin
  Logger.Debug('Load sound from ' + Avalue);
  SndSound := TSndSound.Create;
  SndSound.FileName := Avalue;
  if SndSound.FileName <> '' then
  begin
    Assert(SndSound <> nil);

    Result := SndSound;
    Exit;
  end;
  SndSound.Free;

  MpgSound := TMpgSound.Create;
  MpgSound.FileName := Avalue;

  if MpgSound.FileName <> '' then
  begin
    Assert(MpgSound <> nil);
    Result := MpgSound;

    Exit;
  end;
  MpgSound.Free;

  raise EInvalidAudio.Create('sndfile & mpg123 returned error for ' + AValue);
end;




initialization

  AudioSystem := TAudioSystem.Create;
  if AudioSystem.Loaded then
  begin
    SoundPool := TSoundPool.Create;
  end;


finalization
  //DoneCriticalsection(TAudioPlayer.AudioCriticalSection);
  //SoundPool.Free;
  FreeAndNil(SoundPool);
  //AudioSystem.Free;
  FreeAndNil(AudioSystem);
end.
