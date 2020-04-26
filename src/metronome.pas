unit metronome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, audio, settings;

type

  { TMetronome }

  TMetronome = class(TObject)
    private
      FAudio: TAudio;
      FTickSound: TSndSound;
      FRunning: boolean;
      FLastPlayedTick: longword;
      procedure SetRunning(AValue: boolean);

    public
      constructor Create;
      destructor Destroy; override;
      procedure HandleTimerTrigger;
      property Running: boolean read FRunning write SetRunning;
  end;

implementation

{ TMetronome }

procedure TMetronome.SetRunning(AValue: boolean);
begin
  if FRunning=AValue then Exit;

  FRunning:=AValue;
end;

constructor TMetronome.Create;
begin
  FAudio := TAudio.Create;
  FTickSound := TSndSound.Create;
  FTickSound.LoadTick;

  FLastPlayedTick:=GetTickCount64;
end;

destructor TMetronome.Destroy;
begin
  FTickSound.Free;
  FAudio.Free;
  inherited Destroy;
end;

procedure TMetronome.HandleTimerTrigger;
var
  CurrentTick: longword;
begin
  CurrentTick := GetTickCount64;
  if (CurrentTick - FLastPlayedTick) > 1000 then
  begin
    FAudio.Play(FTickSound, False, GlobalUserConfig.Volume);
    FLastPlayedTick:=CurrentTick;
  end;
end;

end.

