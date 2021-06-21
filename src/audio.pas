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

  { TAudioPlayer }
  TAudioPlayer = class(TObject)
  private

    FUserInfo: TUserInfo;
    FStream: PPaStream;

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
    procedure Abort;
    procedure FinishedAud({%H-}Datax: PtrInt);
    property Playing: boolean read FAudioPlaying;

  end;

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
    procedure LoadSoundFromResource(ResourceName: string; var Sound: TSoundData);

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

var
  PaErrCode: PaError;
  AudioSystem: TAudioSystem = nil;

implementation

//uses
//log;

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
  readSuccess := UsedSound.Read(output, frameCount);

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
      readSuccess := UsedSound.Read(output, frameCount);

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
  FDefaultSound.Loaded := False;
  FDefaultTick.Loaded := False;
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

procedure TAudioSystem.LoadSoundFromResource(ResourceName: string;
  var Sound: TSoundData);
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
  Sound.Loaded := True;
  Stream.Destroy;
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
  LoadSoundFromResource('DEFAULT_SOUND', FDefaultSound);
  LoadSoundFromResource('TICK', FDefaultTick);
end;

procedure TAudioSystem.FreeDefaultSounds;
begin
  FreeMem(FDefaultSound.Buffer);
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


finalization
  //DoneCriticalsection(TAudioPlayer.AudioCriticalSection);

  AudioSystem.Free;
end.
