{

Copyright (C) 2019 Vipin Cherian

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
unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Graphics, DateTimePicker, jsonConf;

type

  { TDefaultConfig }

  {TDefaultConfig = class(TObject)
  private
    FTimerInitMins: Integer;
    FTimerInitSecs: Integer;
    FTimerInitTitle: String;
    FConf: TJSONConfig;
    procedure Load;
    procedure CreateAnew;
  public
    constructor Create();
    Destructor  Destroy; override;
    property TimerInitMins: Integer read FTimerInitMins;
    property TimerInitSecs: Integer read FTimerInitSecs;
    property TimerInitTitle: String read FTimerInitTitle;
  end;}

  { TUserConfig }

  TUserConfig = class(TObject)
  private
    //FFileName: string;
    FDefaultPos: TRect;
    //FLastPosNormal: TRect;

    //FConf: TJSONConfig;
    //procedure Save;
    //procedure CreateAnew;
    function GetDefaultStartPosition: TRect;
    //procedure SetFileName(AValue: string);
  public
    ShowModalAlert: boolean;
    ShowTrayAlert: boolean;
    AutoProgress: boolean;
    LastPosNormal: TRect;
    LastPosRestored: TRect;
    QueryExit: boolean;
    AllowTimerTitleEdit: boolean;
    DefaultTimerTitle: string;
    //DefaultTimerHours: integer;
    //DefaultTimerMins: integer;
    //DefaultTimerSecs: integer;
    DefaultTimerDuration: double;

    LastWindowState: TWindowState;
    DefaultTimeFormat: integer;
    AdjustDiffDefault: double;
    //AdjustExtendDefault: double;
    AdjustCompletebyDefault: double;
    constructor Create();
    destructor Destroy; override;
    //procedure Load;
    //procedure Flush;
    procedure CopyFrom(From: TUserConfig);
    function CompareWith(From: TUserConfig): boolean;
    //property LastPosNormal: TRect read FLastPosNormal write FLastPosNormal;
    //property FileName: string read FFileName write SetFileName;
  end;

  { TUserFileConfig }

  TUserFileConfig = class(TUserConfig)
  private
    FFileName: string;
    FConf: TJSONConfig;
    procedure Save;
    procedure CreateAnew;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure Load;
    procedure Flush;
  end;

var
  //GlobalDefault: TDefaultConfig;
  GlobalUserConfig: TUserFileConfig;

const
  TIMER_INIT_MINS = 'timer_init_mins';
  DEF_TIMER_INIT_MINS = 0;

  TIMER_INIT_SECS = 'timer_init_secs';
  DEF_TIMER_INIT_SECS = 10;

  TIMER_INIT_TITLE = 'timer_init_title';
  DEF_TIMER_INIT_TITLE = 'Countdown timer';

  LAST_POS_TOP = 'x';
  LAST_POS_LEFT = 'y';
  LAST_POS_BOTTOM = 'bottom';
  LAST_POS_RIGHT = 'right';

  LAST_POS_NORMAL = 'last_pos/normal/';
  LAST_POS_RESTORED = 'last_pos/restored/';


  SHOW_MODAL_ALERT = 'show_modal_alert';
  SHOW_TRAY_ALERT = 'show_tray_alert';
  DEF_SHOW_MODAL_ALERT = False;
  DEF_SHOW_TRAY_ALERT = True;

  AUTO_PROGRESS = 'auto_progress';
  DEF_AUTO_PROGRESS = True;

  QUERY_EXIT = 'query_exit';
  DEF_QUERY_EXIT = True;

  ALLOW_TIMERTITLE_EDIT = 'allow_timertitle_edit';
  DEF_ALLOW_TIMERTITLE_EDIT = False;

  TIMER_TITLE = 'timer_title';
  DEF_TIMER_TITLE = 'Countdown timer';

  TIMER_HOURS = 'timer_hours';
  DEF_TIMER_HOURS = 0;

  TIMER_MINS = 'timer_mins';
  DEF_TIMER_MINS = 10;

  TIMER_SECS = 'timer_secs';
  DEF_TIMER_SECS = 0;

  TIMER_DURATION = 'timer_duration';
  DEF_TIMER_DURATION = 3.4722222189884633E-003;

  WINDOW_STATE = 'window_state';
  DEF_WINDOW_STATE = integer(wsNormal);

  TIME_FORMAT = 'time_format';
  DEF_TIME_FORMAT = tf12;

  //ADJ_EXTEND = 'adjust_extend';
  ADJ_DIFF = 'adjust_diff';
  ADJ_COMPLETEBY = 'adust_completeby';

procedure InitSettings;
procedure CleanupSettings;

implementation

procedure InitSettings;
begin
  { Combo-box indices are set blindly. An assert to check this during development. }
  Assert(integer(tf12) = 0);
  Assert(integer(tf24) = 1);

  //DEF_TIMER_DURATION := EncodeTime(0, 5, 0, 0);

  if not DirectoryExists(GetAppConfigDir(False)) then
    CreateDir(GetAppConfigDir(False));
  //GlobalDefault := TDefaultConfig.Create;
  GlobalUserConfig := TUserFileConfig.Create(GetAppConfigDir(False) + 'user.json');
  //GlobalUserConfig.FileName:=GetAppConfigDir(False) + 'user.json';
  GlobalUserConfig.Load;
end;

procedure CleanupSettings;
begin
  GlobalUserConfig.Free;
  //GlobalDefault.Free;
end;



{ TUserFileConfig }

constructor TUserFileConfig.Create(FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  FConf := TJSONConfig.Create(nil);
  FConf.Filename := FFileName;
  FConf.Formatted := True;
end;

destructor TUserFileConfig.Destroy;
begin
  Save;
  FConf.Free;
  inherited Destroy;
end;

procedure TUserFileConfig.Load;
begin

  FConf.Filename := FFileName;
  if not FileExists(FFileName) then
  begin
    CreateAnew;
  end;
  try
    FConf.OpenKey(LAST_POS_NORMAL, False);
    LastPosNormal.Top := FConf.GetValue(LAST_POS_TOP, FDefaultPos.Top);
    LastPosNormal.Left := FConf.GetValue(LAST_POS_LEFT, FDefaultPos.Left);
    LastPosNormal.Right := FConf.GetValue(LAST_POS_RIGHT, FDefaultPos.Right);
    LastPosNormal.Bottom := FConf.GetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
    FConf.CloseKey;

    FConf.OpenKey(LAST_POS_RESTORED, False);
    LastPosRestored.Top := FConf.GetValue(LAST_POS_TOP, FDefaultPos.Top);
    LastPosRestored.Left := FConf.GetValue(LAST_POS_LEFT, FDefaultPos.Left);
    LastPosRestored.Right := FConf.GetValue(LAST_POS_RIGHT, FDefaultPos.Right);
    LastPosRestored.Bottom := FConf.GetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
    FConf.CloseKey;

    LastWindowState := TWindowState(FConf.GetValue(WINDOW_STATE, DEF_WINDOW_STATE));

    {TODO: Is the defaulting correct here?}
    ShowModalAlert := FConf.GetValue(SHOW_MODAL_ALERT, ShowModalAlert);
    ShowTrayAlert := FConf.GetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
    //ShowTrayAlert := FConf.GetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
    AutoProgress := FConf.GetValue(AUTO_PROGRESS, AutoProgress);
    QueryExit := FConf.GetValue(QUERY_EXIT, QueryExit);
    AllowTimerTitleEdit := FConf.GetValue(ALLOW_TIMERTITLE_EDIT, AllowTimerTitleEdit);
    DefaultTimerTitle := string(FConf.GetValue(UTF8Decode(TIMER_TITLE), UTF8Decode(DefaultTimerTitle)));
    //DefaultTimerHours := FConf.GetValue(TIMER_HOURS, DefaultTimerHours);
    //DefaultTimerMins := FConf.GetValue(TIMER_MINS, DefaultTimerMins);
    //DefaultTimerSecs := FConf.GetValue(TIMER_SECS, DefaultTimerSecs);
    DefaultTimerDuration := FConf.GetValue(TIMER_DURATION, DefaultTimerDuration);

    DefaultTimeFormat := FConf.GetValue(TIME_FORMAT, DefaultTimeFormat);

    //AdjustExtendDefault:=FConf.GetValue(ADJ_EXTEND, AdjustExtendDefault);
    AdjustDiffDefault := FConf.GetValue(ADJ_DIFF, AdjustDiffDefault);
    AdjustCompletebyDefault := FConf.GetValue(ADJ_COMPLETEBY, AdjustCompletebyDefault);

  except
    CreateAnew;
  end;

end;

procedure TUserFileConfig.Save;
begin

  FConf.OpenKey(LAST_POS_NORMAL, True);
  FConf.SetValue(LAST_POS_TOP, LastPosNormal.Top);
  FConf.SetValue(LAST_POS_LEFT, LastPosNormal.Left);
  FConf.SetValue(LAST_POS_RIGHT, LastPosNormal.Right);
  FConf.SetValue(LAST_POS_BOTTOM, LastPosNormal.Bottom);
  FConf.CloseKey;

  FConf.OpenKey(LAST_POS_RESTORED, True);
  FConf.SetValue(LAST_POS_TOP, LastPosRestored.Top);
  FConf.SetValue(LAST_POS_LEFT, LastPosRestored.Left);
  FConf.SetValue(LAST_POS_RIGHT, LastPosRestored.Right);
  FConf.SetValue(LAST_POS_BOTTOM, LastPosRestored.Bottom);
  Fconf.CloseKey;

  FConf.SetValue(WINDOW_STATE, integer(LastWindowState));

  FConf.SetValue(SHOW_MODAL_ALERT, ShowModalAlert);
  FConf.SetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
  FConf.SetValue(AUTO_PROGRESS, AutoProgress);
  FConf.SetValue(QUERY_EXIT, QueryExit);
  FConf.SetValue(ALLOW_TIMERTITLE_EDIT, AllowTimerTitleEdit);
  Fconf.SetValue(TIMER_TITLE, UTF8Decode(DefaultTimerTitle));
  //FConf.SetValue(TIMER_HOURS, DefaultTimerHours);
  //FConf.SetValue(TIMER_MINS, DefaultTimerMins);
  //FConf.SetValue(TIMER_SECS, DefaultTimerSecs);
  FConf.SetValue(TIMER_DURATION, DefaultTimerDuration);

  FConf.SetValue(TIME_FORMAT, DefaultTimeFormat);

  //FConf.SetValue(ADJ_EXTEND, AdjustExtendDefault);
  FConf.SetValue(ADJ_DIFF, AdjustDiffDefault);
  FConf.SetValue(ADJ_COMPLETEBY, AdjustCompletebyDefault);

end;

procedure TUserFileConfig.CreateAnew;
begin
  FConf.OpenKey(LAST_POS_NORMAL, True);
  FConf.SetValue(LAST_POS_TOP, FDefaultPos.Top);
  FConf.SetValue(LAST_POS_LEFT, FDefaultPos.Left);
  FConf.SetValue(LAST_POS_RIGHT, FDefaultPos.Right);
  FConf.SetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
  FConf.CloseKey;

  FConf.OpenKey(LAST_POS_RESTORED, True);
  FConf.SetValue(LAST_POS_TOP, FDefaultPos.Top);
  FConf.SetValue(LAST_POS_LEFT, FDefaultPos.Left);
  FConf.SetValue(LAST_POS_RIGHT, FDefaultPos.Right);
  FConf.SetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
  Fconf.CloseKey;

  FConf.SetValue(WINDOW_STATE, DEF_WINDOW_STATE);

  FConf.SetValue(SHOW_MODAL_ALERT, DEF_SHOW_MODAL_ALERT);
  FConf.SetValue(SHOW_TRAY_ALERT, DEF_SHOW_TRAY_ALERT);
  FConf.SetValue(AUTO_PROGRESS, DEF_AUTO_PROGRESS);
  FConf.SetValue(QUERY_EXIT, DEF_QUERY_EXIT);
  Fconf.SetValue(TIMER_TITLE, DEF_TIMER_TITLE);
  FConf.SetValue(TIMER_HOURS, DEF_TIMER_HOURS);
  FConf.SetValue(TIMER_MINS, DEF_TIMER_MINS);
  FConf.SetValue(TIMER_SECS, DEF_TIMER_SECS);

  FConf.SetValue(TIME_FORMAT, integer(DEF_TIME_FORMAT));

  //FConf.SetValue(ADJ_EXTEND, DEF_TIMER_DURATION);
  FConf.SetValue(ADJ_DIFF, DEF_TIMER_DURATION);
  FConf.SetValue(ADJ_COMPLETEBY, DEF_TIMER_DURATION);

  FConf.Flush;
end;

procedure TUserFileConfig.Flush;
begin
  FConf.Flush;
end;


{ TUserConfig }



function TUserConfig.GetDefaultStartPosition: TRect;
var
  Default: TRect;
begin

  Default.Left := Screen.Width div 6;
  Default.Top := Screen.Height div 6;

  Default.Right := Default.Left + ((2 * Screen.Width) div 3);
  Default.Bottom := Default.Top + ((2 * Screen.Height) div 3);

  Result := Default;

end;


constructor TUserConfig.Create;
begin
  //FFileName := GetAppConfigDir(False) + 'user.json';
  //FFileName := '';
  FDefaultPos := GetDefaultStartPosition;
  //FConf := TJSONConfig.Create(nil);
  //FConf := Nil;
  ShowModalAlert := DEF_SHOW_MODAL_ALERT;
  ShowTrayAlert := DEF_SHOW_TRAY_ALERT;
  AutoProgress := DEF_AUTO_PROGRESS;
  QueryExit := DEF_QUERY_EXIT;

  LastWindowState := wsNormal;
  DefaultTimerTitle := DEF_TIMER_TITLE;
  //DefaultTimerHours := DEF_TIMER_HOURS;
  //DefaultTimerMins := DEF_TIMER_MINS;
  //DefaultTimerSecs := DEF_TIMER_SECS;
  DefaultTimerDuration := DEF_TIMER_DURATION;

  DefaultTimeFormat := integer(DEF_TIME_FORMAT);

end;

destructor TUserConfig.Destroy;
begin
  //Save;
  //FConf.Free;
  inherited Destroy;
end;

procedure TUserConfig.CopyFrom(From: TUserConfig);
begin
  ShowModalAlert := From.ShowModalAlert;
  ShowTrayAlert := From.ShowTrayAlert;
  AutoProgress := From.AutoProgress;
  QueryExit := From.QueryExit;

  DefaultTimerTitle := From.DefaultTimerTitle;
  //DefaultTimerHours := From.DefaultTimerHours;
  //DefaultTimerMins := From.DefaultTimerMins;
  //DefaultTimerSecs := From.DefaultTimerSecs;
  DefaultTimerDuration := From.DefaultTimerDuration;
  AllowTimerTitleEdit := From.AllowTimerTitleEdit;
  DefaultTimeFormat := From.DefaultTimeFormat;

  //AdjustExtendDefault:=From.AdjustCompletebyDefault;
  AdjustDiffDefault := From.AdjustDiffDefault;
  AdjustCompletebyDefault := From.AdjustCompletebyDefault;
end;

function TUserConfig.CompareWith(From: TUserConfig): boolean;
begin
  if ShowModalAlert <> From.ShowModalAlert then
  begin
    Result := False;
    Exit;
  end;
  if ShowTrayAlert <> From.ShowTrayAlert then
  begin
    Result := False;
    Exit;
  end;
  if AutoProgress <> From.AutoProgress then
  begin
    Result := False;
    Exit;
  end;
  if QueryExit <> From.QueryExit then
  begin
    Result := False;
    Exit;
  end;
  if DefaultTimerTitle <> From.DefaultTimerTitle then
  begin
    Result := False;
    Exit;
  end;
  {if DefaultTimerHours <> From.DefaultTimerHours then
  begin
    Result := False;
    Exit;
  end;
  if DefaultTimerMins <> From.DefaultTimerMins then
  begin
    Result := False;
    Exit;
  end;
  if DefaultTimerSecs <> From.DefaultTimerSecs then
  begin
    Result := False;
    Exit;
  end;}

  if DefaultTimerDuration <> From.DefaultTimerDuration then
  begin
    Result := False;
    Exit;
  end;
  if DefaultTimeFormat <> From.DefaultTimeFormat then
  begin
    Result := False;
    Exit;
  end;

  {if AdjustExtendDefault <> From.AdjustExtendDefault then
  begin
    Result := False;
    Exit;
  end;}
  if AdjustDiffDefault <> From.AdjustDiffDefault then
  begin
    Result := False;
    Exit;
  end;
  if AdjustCompletebyDefault <> From.AdjustCompletebyDefault then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

{ TDefaultConfig }
{
procedure TDefaultConfig.Load;
var
  FileName: String;
begin
  FConf.Filename:= GetAppConfigDir(False) + 'override.json';
  FileName := GetAppConfigDir(False) + 'override.json';
  if not FileExists(FileName)
  then
    CreateAnew;
  //else
    //;//TODO: Verify

  FTimerInitMins := FConf.GetValue(TIMER_INIT_MINS, DEF_TIMER_INIT_MINS);
  FTimerInitSecs := FConf.GetValue(TIMER_INIT_SECS, DEF_TIMER_INIT_SECS);
  FTimerInitTitle := FConf.GetValue(TIMER_INIT_TITLE, DEF_TIMER_INIT_TITLE);
end;

procedure TDefaultConfig.CreateAnew;
begin
  FConf.SetValue(TIMER_INIT_MINS, DEF_TIMER_INIT_MINS);
  FConf.SetValue(TIMER_INIT_SECS, DEF_TIMER_INIT_SECS);
  FConf.SetValue(TIMER_INIT_TITLE, DEF_TIMER_INIT_TITLE);
  FConf.SetValue('haha/hihi', 0);
  FConf.Flush;
end;

constructor TDefaultConfig.Create;
begin
  FConf := TJSONConfig.Create(nil);
  Load;
end;

destructor TDefaultConfig.Destroy;
begin
  FConf.Free;
  inherited Destroy;
end; }

end.






