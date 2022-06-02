{

Copyright (C) 2021 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}

unit metronome;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, Forms, audio, settings, ExtCtrls, log, constants;

type

  { TMetronome }

  TMetronome = class(TObject)
  private
    FAudioPlayer: TAudioPlayer;
    FRunning: boolean;
    FSubscribers: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure Start;
    procedure Stop;
    property Running: boolean read FRunning;
  end;

var
  MetronomeInstance: TMetronome = nil;

implementation

{ TMetronome }

constructor TMetronome.Create;
begin
  { Constructor cannot be hidden unless it is made strict private.
  Constructor will proceed with creation only if the single instance is not
  created yet. Once created, the same instance is returned.}
  if not Assigned(MetronomeInstance) then
  begin
    inherited Create;
    FAudioPlayer := TAudioPlayer.Create;
    FAudioPlayer.Looped := True;
    FSubscribers := 0;
    FRunning := False;
  end
  else
    Self := MetronomeInstance;

end;

destructor TMetronome.Destroy;
begin
  if FAudioPlayer.Playing then
    Abort;
  FAudioPlayer.Free;
  inherited Destroy;
end;

procedure TMetronome.Abort;
var
  StartTickCount: QWord;
begin

  FAudioPlayer.Abort;

  StartTickCount := GetTickCount64;
  while FAudioPlayer.Playing do
  begin
    Logger.Debug('Waiting for frame metronome to stop audio');
    Application.ProcessMessages;
    if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
      break;
  end;
end;

procedure TMetronome.Start;
begin
  if not FAudioPlayer.Playing then
  begin
    Logger.Debug('When metronome started, FSubscribers - ' + IntToStr(FSubscribers));
    Assert(FSubscribers = 0);
    FAudioPlayer.Play(SoundPool.RawTickSound);
    Logger.Info('Metronome started.');
  end;
  Inc(FSubscribers);
end;

procedure TMetronome.Stop;
begin
  if FSubscribers > 0 then
  begin
    Assert(FAudioPlayer.Playing);
    FAudioPlayer.Abort;
    Logger.Info('Metronome stopped.');
  end;
  if FSubscribers > 0 then
    Dec(FSubscribers);
end;


initialization;
  if AudioSystem.Loaded then
  begin
    MetronomeInstance := TMetronome.Create;
  end;

finalization;
  FreeAndNil(MetronomeInstance);

end.
