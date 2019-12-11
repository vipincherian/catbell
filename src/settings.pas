{

Copyright (C) 2017 Vipin Cherian

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

}
unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Graphics, jsonConf;

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
    FLastPos: TRect;

    //FConf: TJSONConfig;
    //procedure Save;
    //procedure CreateAnew;
    function GetDefaultStartPosition: TRect;
    //procedure SetFileName(AValue: string);
  public
    ShowModalAlert: boolean;
    ShowTrayAlert: boolean;
    AutoProgress: boolean;
    QueryExit: boolean;
    AllowTimerTitleEdit: boolean;
    DefaultTimerTitle: string;
    DefaultTimerHours: integer;
    DefaultTimerMins: integer;
    DefaultTimerSecs: integer;
    ModalBackgroundColour: integer;
    ModalCaptionColour: integer;
    ModalSubtextColour: integer;
    constructor Create();
    destructor Destroy; override;
    //procedure Load;
    //procedure Flush;
    procedure CopyFrom(From: TUserConfig);
    function CompareWith(From: TUserConfig): boolean;
    property LastPos: TRect read FLastPos write FLastPos;
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

  LAST_POS_TOP = 'last_pos_x';
  LAST_POS_LEFT = 'last_pos_y';
  LAST_POS_BOTTOM = 'last_pos_bottom';
  LAST_POS_RIGHT = 'last_pos_right';


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

  MODAL_BKG_COLOUR = 'modal_bkg_colour';
  DEF_MODAL_BKG_COLOUR = clBlack;

  MODAL_CAPTION_COLOUR = 'modal_caption_colour';
  DEF_MODAL_CAPTION_COLOUR = clLime;

  MODAL_SUB_COLOUR = 'modal_sub_colour';
  DEF_MODAL_SUB_COLOUR = clSilver;


procedure InitSettings;
procedure CleanupSettings;

implementation

procedure InitSettings;
begin
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
  FConf := TJSONConfig.Create(Nil);
  FConf.Filename:=FFileName;
  FConf.Formatted:=true;
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
    FLastPos.Top := FConf.GetValue(LAST_POS_TOP, FDefaultPos.Top);
    FLastPos.Left := FConf.GetValue(LAST_POS_LEFT, FDefaultPos.Left);
    FLastPos.Right := FConf.GetValue(LAST_POS_RIGHT, FDefaultPos.Right);
    FLastPos.Bottom := FConf.GetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
    ShowModalAlert := FConf.GetValue(SHOW_MODAL_ALERT, ShowModalAlert);
    ShowTrayAlert := FConf.GetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
    //ShowTrayAlert := FConf.GetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
    AutoProgress := FConf.GetValue(AUTO_PROGRESS, AutoProgress);
    QueryExit := FConf.GetValue(QUERY_EXIT, QueryExit);
    AllowTimerTitleEdit:=FConf.GetValue(ALLOW_TIMERTITLE_EDIT, AllowTimerTitleEdit);
    DefaultTimerTitle := FConf.GetValue(TIMER_TITLE, DefaultTimerTitle);
    DefaultTimerHours := FConf.GetValue(TIMER_HOURS, DefaultTimerHours);
    DefaultTimerMins := FConf.GetValue(TIMER_MINS, DefaultTimerMins);
    DefaultTimerSecs := FConf.GetValue(TIMER_SECS, DefaultTimerSecs);

    ModalBackgroundColour := FConf.GetValue(MODAL_BKG_COLOUR, ModalBackgroundColour);
    ModalCaptionColour := FConf.GetValue(MODAL_CAPTION_COLOUR, ModalCaptionColour);
    ModalSubtextColour := FConf.GetValue(MODAL_SUB_COLOUR, ModalSubtextColour);
  except
    CreateAnew;
  end;

end;

procedure TUserFileConfig.Save;
begin
  FConf.SetValue(LAST_POS_TOP, FLastPos.Top);
  FConf.SetValue(LAST_POS_LEFT, FLastPos.Left);
  FConf.SetValue(LAST_POS_RIGHT, FLastPos.Right);
  FConf.SetValue(LAST_POS_BOTTOM, FLastPos.Bottom);
  FConf.SetValue(SHOW_MODAL_ALERT, ShowModalAlert);
  FConf.SetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
  FConf.SetValue(AUTO_PROGRESS, AutoProgress);
  FConf.SetValue(QUERY_EXIT, QueryExit);
  FConf.SetValue(ALLOW_TIMERTITLE_EDIT, AllowTimerTitleEdit);
  Fconf.SetValue(TIMER_TITLE, DefaultTimerTitle);
  FConf.SetValue(TIMER_HOURS, DefaultTimerHours);
  FConf.SetValue(TIMER_MINS, DefaultTimerMins);
  FConf.SetValue(TIMER_SECS, DefaultTimerSecs);
  FConf.SetValue(MODAL_BKG_COLOUR, ModalBackgroundColour);
  FConf.setValue(MODAL_CAPTION_COLOUR, ModalCaptionColour);
  FConf.setValue(MODAL_SUB_COLOUR, ModalSubtextColour);
end;

procedure TUserFileConfig.CreateAnew;
begin
  FConf.SetValue(LAST_POS_TOP, FDefaultPos.Top);
  FConf.SetValue(LAST_POS_LEFT, FDefaultPos.Left);
  FConf.SetValue(LAST_POS_RIGHT, FDefaultPos.Right);
  FConf.SetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
  FConf.SetValue(SHOW_MODAL_ALERT, DEF_SHOW_MODAL_ALERT);
  FConf.SetValue(SHOW_TRAY_ALERT, DEF_SHOW_TRAY_ALERT);
  FConf.SetValue(AUTO_PROGRESS, DEF_AUTO_PROGRESS);
  FConf.SetValue(QUERY_EXIT, DEF_QUERY_EXIT);
  Fconf.SetValue(TIMER_TITLE, DEF_TIMER_TITLE);
  FConf.SetValue(TIMER_HOURS, DEF_TIMER_HOURS);
  FConf.SetValue(TIMER_MINS, DEF_TIMER_MINS);
  FConf.SetValue(TIMER_SECS, DEF_TIMER_SECS);
  FConf.SetValue(MODAL_BKG_COLOUR, DEF_MODAL_BKG_COLOUR);
  FConf.setValue(MODAL_CAPTION_COLOUR, DEF_MODAL_CAPTION_COLOUR);
  FConf.setValue(MODAL_SUB_COLOUR, DEF_MODAL_SUB_COLOUR);

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
  DefaultTimerTitle := DEF_TIMER_TITLE;
  DefaultTimerHours := DEF_TIMER_HOURS;
  DefaultTimerMins := DEF_TIMER_MINS;
  DefaultTimerSecs := DEF_TIMER_SECS;
  ModalBackgroundColour := DEF_MODAL_BKG_COLOUR;
  ModalCaptionColour := DEF_MODAL_CAPTION_COLOUR;
  ModalSubtextColour := DEF_MODAL_SUB_COLOUR;
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
  DefaultTimerHours := From.DefaultTimerHours;
  DefaultTimerMins := From.DefaultTimerMins;
  DefaultTimerSecs := From.DefaultTimerSecs;
  ModalBackgroundColour := From.ModalBackgroundColour;
  ModalCaptionColour := From.ModalCaptionColour;
  ModalSubtextColour := From.ModalSubtextColour;
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
  if DefaultTimerHours <> From.DefaultTimerHours then
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
  end;
  if ModalBackgroundColour <> From.ModalBackgroundColour then
  begin
    Result := False;
    Exit;
  end;
  if ModalCaptionColour <> From.ModalCaptionColour then
  begin
    Result := False;
    Exit;
  end;
  if ModalSubtextColour <> From.ModalSubtextColour then
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






