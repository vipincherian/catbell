unit audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sndfile, portaudio, LazLogger, ctypes, Forms,
  Dialogs, LCLIntf, lcltype;

const
  READ_NOTLOADED = -1;
  READ_SND = 0;

type
  EAudioNotLoaded = class(Exception);
  EInvalidDevice = class(Exception);
  EInvalidAudio = class(Exception);
  AudioDeviceIndex = PaDeviceIndex;

  TUserInfo = record
    //name : string[30];
    //age  : byte;
    SoundFile: PSndFile;
    Info: SF_INFO;
    //Handle: THandle;
    Player: Pointer;
    Looped: boolean;
    //CriticalSection: TRTLCriticalSection;
  end;
  PAudioInfo = ^TUserInfo;

  { TAudio }
  TAudio = class(TObject)
  private
    FSoundFile: PSNDFILE;
    FFileType: integer;

    FFileName: string;
    FAudioLength: double;

    FInfo: SF_INFO;
    FUserInfo: TUserInfo;
    FStream: PPaStream;

    FAudioPlaying: boolean;

    FOutputDevice: string;

    //PlayCriticalSection: TRTLCriticalSection;

    //FOwner: TForm;

    class function GetDevices: TStringList; static;
    procedure SetFileName(AValue: string);
    procedure SetOutputDevice(AValue: string);
  public
    Loaded: boolean; static;
    FDeviceNames: TStringList; static;
    FDefaultDevice: integer; static;
    AudioCriticalSection: TRTLCriticalSection; static;
    OnPlayCompletion: TNotifyEvent;
    Looped: boolean;
    constructor Create();
    destructor Destroy; override;
    class function GetDefaultDevice: AudioDeviceIndex; static;
    class function GetDeviceIndex(DeviceName: string): AudioDeviceIndex; static;
    //class procedure GetAllDevices
    class property DefaultDevice: AudioDeviceIndex read GetDefaultDevice;
    class property Devices: TStringList read GetDevices;
    procedure Play;
    procedure Abort;
    procedure FinishedAud(Data: PtrInt);
    property FileName: string read FFileName write SetFileName;
    property Duration: double read FAudioLength;
    property Playing: boolean read FAudioPlaying;
    property OutputDevice: string read FOutputDevice write SetOutputDevice;

  end;


implementation

{This function is called by PortAudio to request for audio data, and is passed
as a parameter while opening the stream.
It tries to read from the sound file and supply the data.
If read does not return any data, or if it is less than the number of frames
requested, it assumes tha the entire data has been exhausted and
it closes the sound file and returns paComplete.
This will stop the stream and the associated callback - which
triggers on stoppage of the steam - gets called.}

function FeedAudioStream(input: pointer; output: pointer; frameCount: culong;
  timeInfo: PPaStreamCallbackTimeInfo; statusFlags: PaStreamCallbackFlags;
  userData: pointer): cint; cdecl;
var
  AudioInfo: PAudioInfo;
  //AudBuffer: pointer;
  readCount: cint;
  //Widget: TfraTimer;
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);
  //DebugLn('Inside audio callback1');
  AudioInfo := PAudioinfo(userData);

  //Widget := TfraTimer(AudioInfo^.Widget);

  readCount := 0;
  readCount := sf_read_float(AudioInfo^.SoundFile, output, frameCount *
    (AudioInfo^.Info.channels));

  if AudioInfo^.Looped then
  begin
    { If audio is lopped and if no audio data could be read,
    seek to the beginning and then read again }
    if readCount = 0 then
    begin
      if sf_seek(AudioInfo^.SoundFile, 0, SEEK_SET) = -1 then
      begin
        DebugLn('Sf_seek returned error');
      end;
      readCount := 0;
      readCount := sf_read_float(AudioInfo^.SoundFile, output, frameCount *
        (AudioInfo^.Info.channels));
      if readCount = 0 then
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
    if readCount = (frameCount * AudioInfo^.Info.channels) then
    begin
      Result := cint(paContinue);
    end
    else
    begin
      //sf_close(AudioInfo^.SoundFile);
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
  AudioTemp: TAudio;
begin
  EnterCriticalSection(TAudio.AudioCriticalSection);
  //DebugLn('Inside streamFinished1');
  AudioInfo := PAudioinfo(userData);

  AudioTemp := TAudio(AudioInfo^.Player);
  //PostMessage(AudioInfo^.Handle, UM_FINISHED_AUDIO, 0, 0);
  Application.QueueAsyncCall(@(AudioTemp.FinishedAud), 0);
  LeaveCriticalSection(TAudio.AudioCriticalSection);
end;

{ TAudio }

class function TAudio.GetDevices: TStringList; static;
var
  NumDevices, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  DeviceName: string;
begin
  EnterCriticalSection(AudioCriticalSection);
  if not TAudio.Loaded then
  begin
    LeaveCriticalSection(AudioCriticalSection);
    raise EAudioNotLoaded.Create('Audio not loaded.');
  end;

  TAudio.FDeviceNames.Clear;
  NumDevices := Pa_GetDeviceCount();
  if NumDevices < 0 then
  begin
    DebugLn('Pa_GetDeviceCount failed ');
    DebugLn('Error after Pa_GetDeviceCount ' + IntToStr(NumDevices));
  end;

  {DefaultDeviceId := Pa_GetDefaultOutputDevice();
  if DefaultDeviceId = paNoDevice then
  begin
    DebugLn('No default device');
  end;
  DebugLn('Default device is ' + IntToStr(DefaultDeviceId)); }

  for Count := 0 to NumDevices - 1 do
  begin
    DeviceInfo := Pa_GetDeviceInfo(Count);
    if DeviceInfo = nil then
    begin
      DebugLn('Error after GetDeviceInfo for device #' + IntToStr(Count));
      //FDeviceNames.Clear;
      FDeviceNames.Add('Unknown');
    end
    else
    begin
      DeviceName := StrPas(DeviceInfo^.Name);
      FDeviceNames.Add(DeviceName);
    end;

  end;
  Result := FDeviceNames;
  LeaveCriticalSection(AudioCriticalSection);
end;

procedure TAudio.SetFileName(AValue: string);
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
    //sf_perror(nil);
    //ReadKey;
    //exit;
    //Error := 'SoundFile is nil';
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
end;

procedure TAudio.SetOutputDevice(AValue: string);
var
  Id: AudioDeviceIndex;
begin
  try
    Id := GetDeviceIndex(Avalue);
    FOutputDevice:=AValue;
  finally
  end;
end;



constructor TAudio.Create();
begin

  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');
  FSoundFile := nil;
  FFileType := READ_NOTLOADED;

  FFileName := '';
  Looped := False;
  FAudioPlaying := False;

  OnPlayCompletion := nil;

  //InitCriticalSection(CallbackCriticalSection);
  //InitializeCriticalSection(AudioCriticalSection);
  //FOwner := Nil;

  //FOwner.Application;
  //Application.QueueAsyncCall();
end;

destructor TAudio.Destroy;
begin
  DebugLn('TAudio.Destroy ');
  //DeleteCriticalsection(AudioCriticalSection);
  //DoneCriticalSection(CallbackCriticalSection);
  inherited Destroy;
end;

class function TAudio.GetDefaultDevice: AudioDeviceIndex;
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

class function TAudio.GetDeviceIndex(DeviceName: string): AudioDeviceIndex;
var
  NumDevices, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  //DeviceName: string;
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

  {DefaultDeviceId := Pa_GetDefaultOutputDevice();
  if DefaultDeviceId = paNoDevice then
  begin
    DebugLn('No default device');
  end;
  DebugLn('Default device is ' + IntToStr(DefaultDeviceId)); }

  for Count := 0 to NumDevices - 1 do
  begin
    DeviceInfo := Pa_GetDeviceInfo(Count);
    if DeviceInfo = nil then
    begin
      DebugLn('Error after GetDeviceInfo for device #' + IntToStr(Count));
      //FDeviceNames.Clear;
      //FDeviceNames.Add('Unknown');
    end
    else
    if DeviceName = StrPas(DeviceInfo^.Name) then
    begin
      Result := Count;
      LeaveCriticalSection(AudioCriticalSection);
      Exit;
    end;
  end;
  LeaveCriticalSection(AudioCriticalSection);
  raise EInvalidDevice.Create('No matching device for ' + DeviceName);

end;

procedure TAudio.Play;
var
  //SoundFile: PSndFile;
  //Info: SF_INFO;
  PaErrCode: PaError;
  StreamParams: PaStreamParameters;
  NumDevices, DeviceId, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  DeviceName: string;
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

    if sf_seek(FSoundFile, 0, SEEK_SET) = -1 then
    begin
      DebugLn('Sf_seek returned error');
    end;

    if FOutputDevice = '' then
      DeviceId := DefaultDevice
    else
      DeviceId := GetDeviceIndex(FOutputDevice);
    StreamParams.device := DeviceId;

    DebugLn('Audio device is ' + IntToStr(StreamParams.device));

    StreamParams.channelCount := FInfo.channels;
    StreamParams.sampleFormat := paFloat32;

    Streamparams.suggestedLatency :=
      Pa_GetDeviceInfo(StreamParams.device)^.defaultLowOutputLatency;
    StreamParams.hostApiSpecificStreamInfo := nil;
    DebugLn('Default device is ' + IntToStr(StreamParams.device));

    //Callback := @FeedAudioStream;
    FUserInfo.SoundFile := FSoundFile;
    //FUserInfo.Handle := Handle;
    //FUserInfo.Widget := Self;
    FUserInfo.Looped := Looped;
    FUserInfo.Player := Self;
    //FUserInfo.CriticalSection:=AudioCriticalSection;
    Move(FInfo, FUserInfo.Info, SizeOf(SF_INFO));

    PaErrCode := Pa_OpenStream(@FStream, nil, @StreamParams, FInfo.samplerate,
      paFramesPerBufferUnspecified, paClipOff, PPaStreamCallback(@FeedAudioStream),
      @FUserInfo);
    if (paErrCode <> Int32(paNoError)) then
    begin
      DebugLn('Pa_OpenStream failed ' + Pa_GetErrorText(paErrCode));
      DebugLn('Error after Pa_OpenStream ' + IntToHex(PaErrCode, 8));
    end;

    PaErrCode := Pa_SetStreamFinishedCallback(FStream,
      PPaStreamFinishedCallback(@AudioStreamFinished));
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

  finally
    //LeaveCriticalSection(AudioCriticalSection);
  end;


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
      DebugLn('Audio not playing. There is nothing to be done in abort');
      Exit;
    end;
    PaErrCode := Pa_AbortStream(FStream);
    if (paErrCode <> Int32(paNoError)) then
    begin
      WriteLn('Pa_AbortStream failed ' + Pa_GetErrorText(paErrCode));
      WriteLn('Error after Pa_AbortStream ' + IntToHex(PaErrCode, 8));
    end;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}

  finally
    //FAudioPlaying := False;
    //LeaveCriticalSection(AudioCriticalSection);
  end;

end;

procedure TAudio.FinishedAud(Data: PtrInt);
var
  PaErrCode: PaError;
begin
  if not TAudio.Loaded then
    raise EAudioNotLoaded.Create('Audio not loaded.');

  //EnterCriticalSection(AudioCriticalSection);

  {This check might be redundant. Just to be safe}

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
  //LeaveCriticalSection(AudioCriticalSection);
end;

var
  PaErrCode: PaError;


initialization
  TAudio.Loaded := False;
  TAudio.FDeviceNames := TStringList.Create;
  {$IFNDEF AUDIO_STATIC}
  TAudio.Loaded := Pa_Load(LIB_PORTAUDIO);
  if not TAudio.Loaded then
  begin
    DebugLn('Could not load portaudio');
    //ReadKey;
    Exit;
  end;

  //FAudioWorking:=Status;

  { Load sndfile library only if portaudio was loaded successfully }

  if TAudio.Loaded then
  begin
    TAudio.Loaded := sf_load(LIB_SNDFILE);
    if not TAudio.Loaded then
    begin
      DebugLn('Could not load sndfile');
      //ReadKey;
      //exit;
      Pa_Unload();
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
    {$IFNDEF AUDIO_STATIC}
    Sf_Unload;
    Pa_Unload;
    {$ENDIF}
  end;
  TAudio.FDeviceNames.Free;
end.
