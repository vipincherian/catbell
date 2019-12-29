unit audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sndfile, portaudio, LazLogger, ctypes;

const
  READ_NOTLOADED = -1;
  READ_SND = 0;

type
  EAudioNotLoaded = Class(Exception);
  ENoDefaultDevice = Class(Exception);
  AudioDeviceIndex = PaDeviceIndex;
  { TAudio }
  TAudio = class(TObject)
  private
    FSndFile: PSNDFILE;
    FFileType: integer;
    class function GetDevices: TStringList; static;
  public
    AudioLoaded: boolean; static;
    FDeviceNames: TStringList; static;
    constructor Create();
    destructor Destroy; override;
    class function GetDefaultDevice: AudioDeviceIndex; static;
    class function GetDevice(DeviceName: string): AudioDeviceIndex; static;
    //class procedure GetAllDevices
    class property DefaultDevice: AudioDeviceIndex read GetDefaultDevice;
    class property Devices: TStringList read GetDevices;
  end;

var
  PaErrCode: PaError;
  DefaultDevice: integer;

implementation

{ TAudio }

class function TAudio.GetDevices: TStringList; static;
var
  NumDevices, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  DeviceName: string;
begin
  if not TAudio.AudioLoaded then
  begin
    ;
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
      FDeviceNames.Clear;
    end
    else
    begin
      DeviceName := StrPas(DeviceInfo^.Name);
      FDeviceNames.Add(DeviceName);
    end;

  end;
  Result := FDeviceNames;
end;



constructor TAudio.Create();
begin
  FSndFile := nil;
  FFileType := READ_NOTLOADED;

end;

destructor TAudio.Destroy;
begin

  inherited Destroy;
end;

class function TAudio.GetDefaultDevice: AudioDeviceIndex;
var
  DeviceId: AudioDeviceIndex;
begin
  DeviceId := Pa_GetDefaultOutputDevice();
  if DeviceId = paNoDevice then
  begin
    DebugLn('No default device');
    Raise ENoDefaultDevice.Create('Pa_GetDefaultOutputDevice() returned PaNoDevice');
  end;
  Result := DeviceId;
end;

class function TAudio.GetDevice(DeviceName: string): AudioDeviceIndex;
begin

end;

initialization
  TAudio.AudioLoaded := False;
  TAudio.FDeviceNames := TStringList.Create;
  {$IFNDEF AUDIO_STATIC}
  TAudio.AudioLoaded := Pa_Load(LIB_PORTAUDIO);
  if not TAudio.AudioLoaded then
  begin
    DebugLn('Could not load portaudio');
    //ReadKey;
    Exit;
  end;

  //FAudioWorking:=Status;

  { Load sndfile library only if portaudio was loaded successfully }

  if TAudio.AudioLoaded then
  begin
    TAudio.AudioLoaded := sf_load(LIB_SNDFILE);
    if not TAudio.AudioLoaded then
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

  if TAudio.AudioLoaded then
  begin
    PaErrCode := Pa_Initialize();
    if PaErrCode <> cint(paNoError) then
    begin
      DebugLn('Error in Pa_Initialize()');

      TAudio.AudioLoaded := False;

      { If portaudio cannot be initialised, then audio will not work.
      Unload libraries }
      {$IFNDEF AUDIO_STATIC}
      sf_Unload;
      Pa_Unload;
      {$ENDIF}
    end;
  end;

  if TAudio.AudioLoaded then
  begin
    DefaultDevice := Pa_GetDefaultOutputDevice();
    if DefaultDevice = paNoDevice then
    begin
      DebugLn('No default device');
      TAudio.AudioLoaded := False;
      Pa_Terminate;
      {$IFNDEF AUDIO_STATIC}
      sf_Unload;
      Pa_Unload;
      {$ENDIF}
    end;
  end;



finalization
  if TAudio.AudioLoaded then
  begin
    Pa_Terminate;
    {$IFNDEF AUDIO_STATIC}
    Sf_Unload;
    Pa_Unload;
    {$ENDIF}
  end;
  TAudio.FDeviceNames.Free;
end.
