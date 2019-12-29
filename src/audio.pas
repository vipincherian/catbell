unit audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sndfile, portaudio, LazLogger, ctypes;
const
  READ_NOTLOADED = -1;
  READ_SND = 0;
type

  { TAudio }
  TAudio = class(TObject)
  private
    FSndFile: PSNDFILE;
    FFileType: integer;
  public
    AudioLoaded: boolean; static;
    constructor Create();
    destructor Destroy; override;
   end;

var
  PaErrCode: PaError;
  DefaultDevice: integer;

implementation

{ TAudio }

constructor TAudio.Create();
begin
  FSndFile := nil;
  FFileType:=READ_NOTLOADED;
end;

destructor TAudio.Destroy;
begin
  inherited Destroy;
end;

initialization
  TAudio.AudioLoaded := False;

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
      sf_Unload();
      Pa_Unload();
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
      {$IFNDEF AUDIO_STATIC}
      sf_Unload();
      Pa_Unload();
      {$ENDIF}
    end;
  end;



finalization
  if TAudio.AudioLoaded then
    Pa_Terminate;
{$IFNDEF AUDIO_STATIC}
  Sf_Unload;
  Pa_Unload;
{$ENDIF}
end.
