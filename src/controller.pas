{

Copyright (C) 2017 Vipin Cherian

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
unit controller;

{$mode objfpc}{$H+}
//{$INTERFACES CORBA}
interface

uses
  Classes, SysUtils, Dialogs, jsonConf, dateutils, clocks, clockswidget,
  main, observers;

type

  { TController }

  TController = class(TInterfacedObject)

  private
    FClocks: TClocks;
    FFormWidget: TMainForm;
    FDbFileName: string;
    FDbDefault: boolean;

    procedure NewTimerAdded(Sender: TObject);
    procedure SetForm(AValue: TMainForm);
    procedure ClockDeleted(Sender: TObject);
    procedure ClockSelected(Sender: TObject);
    procedure ClocksMovedUp(Sender: TObject);
    procedure ClocksMovedDown(Sender: TObject);
    procedure ExportToFile(Sender: TObject);
    procedure PostTimerCreation(AValue: TTimerClock);
    procedure SetListButtonsStatus;

  public
    constructor Create();
    destructor Destroy; override;
    procedure ProcessCommandline;
    procedure SavetoFile;
    procedure LoadfromFile;
    property Clocks: TClocks read FClocks;
    property FormWidget: TMainForm read FFormWidget write SetForm;
    //property ListButtonsEnabled: boolean write SetListButtonsEnabled;
  end;

implementation

{ TController }


procedure TController.NewTimerAdded(Sender: TObject);
var
  Added: TTimerClock;
begin

  Added := FClocks.AddTimer;
  PostTimerCreation(Added);
  //Added.AddSubscription(FFormWidget);
  //Added.Widget.OnSelect:=@ClockSelected;
  //FClocks.Widget.Reorder;
end;


procedure TController.SetForm(AValue: TMainForm);
begin
  if FFormWidget = AValue then
    Exit;
  FFormWidget := AValue;

  AVAlue.OnNewTimer := @NewTimerAdded;
  //AValue.OnNewAlarm:=@NewAlarmAdded;
  AValue.OnClockDelete := @ClockDeleted;
  AValue.OnClockMoveUp := @ClocksMovedUp;
  AValue.OnClockMoveDown := @ClocksMovedDown;
  AValue.OnEXport := @ExportToFile;
  FClocks.Widget := AVAlue.Clocks;
end;

procedure TController.ClockDeleted(Sender: TObject);
begin
  //ShowMessage('Inside TController.ClockDeleted');
  FClocks.DeleteSelected;

  SetListButtonsStatus;
end;

procedure TController.ClockSelected(Sender: TObject);
begin
  //ListButtonsEnabled := FClocks.AnySelected;
  SetListButtonsStatus;
end;

procedure TController.ClocksMovedUp(Sender: TObject);
begin
  FClocks.Widget.MoveSelectedClocksUp;
  SetListButtonsStatus;
end;

procedure TController.ClocksMovedDown(Sender: TObject);
begin
  FClocks.Widget.MoveSelectedClocksDown;
  SetListButtonsStatus;
end;

procedure TController.ExportToFile(Sender: TObject);
var
  FileName: string;
  Conf: TJSONConfig;
begin
  SavetoFile;
  FileName := '';
  FileName := FFormWidget.GetExportFileName;
  if FileName <> '' then
  begin
    Conf := TJSONConfig.Create(nil);
    FClocks.SaveClocks(Conf);
    Conf.Filename := FileName;
    Conf.Free;
  end;
end;

procedure TController.PostTimerCreation(AValue: TTimerClock);
begin
  AValue.AddSubscription(FFormWidget);
  AValue.Widget.OnSelect := @ClockSelected;
end;


procedure TController.SetListButtonsStatus;
begin
  with FFormWidget do
  begin
    DeleteEnabled := FClocks.AnySelected;
    MoveDownEnabled := FClocks.Widget.CanSelectedMovDown;
    MoveUpEnabled := FClocks.Widget.CanSelectedMoveUp;
  end;
end;


constructor TController.Create;
begin
  FClocks := TClocks.Create;
  FDbDefault := True;

end;

destructor TController.Destroy;
begin
  SavetoFile;
  FClocks.Destroy;
  inherited Destroy;
end;

procedure TController.ProcessCommandline;
var
  DefaultDbFile: string;
  PassedDbFile: string;
  //Conf: TJSONConfig;
begin
  DefaultDbFile := GetAppConfigDir(False) + 'defdb.ccq';
  FDbFileName := DefaultDbFile;
  FDbDefault := True;
  if ParamCount > 0 then
  begin
    PassedDbFile := ParamStr(1);
    if FileExists(PassedDbFile) then
      FDbFileName := PassedDbFile;
  end;

end;

procedure TController.SavetoFile;
var
  Conf: TJSONConfig;
  //Count: integer;
begin
  //Conf.SetValue(TIMER_TITLE, );
  Conf := TJSONConfig.Create(nil);
  Conf.Formatted:=true;
  FClocks.SaveClocks(Conf);
  Conf.Filename := FDbFileName;
  //Conf.Flush;
  Conf.Free;
end;

procedure TController.LoadfromFile;
var
  Conf: TJSONConfig;
  TotalCount, Count: integer;
  NewTimerClock: TTimerClock;
  Hours, Mins, Secs: word;
  Order: TIdList;
  OrderString: TStringList;
  Pos: string;
begin
  Order := nil;
  OrderString := nil;

  if FileExists(FDbFileName) then
  begin

    Conf := TJSONConfig.Create(nil);
    Conf.FileName := FDbFileName;
    //FClocks.LoadClocks(Conf);
    TotalCount := Conf.GetValue(TIMER_CONF_COUNT, 0);
    for Count := 0 to TotalCount - 1 do
    begin
      NewTimerClock := FClocks.AddTimer;
      NewTimerClock.Widget.Caption :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_TITLE, 'Countdown timer');
      Hours := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_HOURS, 0);
      Mins := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_MINUTES, 0);
      Secs := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_SECONDS, 0);
      //TODO: Remove hardcoding
      NewTimerClock.Widget.Duration :=
        EncodeDateTime(2000, 1, 1, Hours, Mins, Secs, 0);
      NewTimerClock.Notifier :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_NOTIFIER, False);
      //NewTimerClock.AddSubscription(FFormWidget);
      //NewTimerClock.Widget.OnSelect:=@ClockSelected;
      PostTimerCreation(NewTimerClock);
    end;
    Order := TIdList.Create;
    OrderString := TStringList.Create;

    if not Conf.GetValue(TIMER_CONF_ORDER, OrderString, '0') then
      ShowMessage('Getting order failed');

    for Pos in OrderString do
    begin
      Order.Add(StrToInt(Pos));
    end;

    FClocks.Widget.SetOrder(Order);

    Conf.Free;
    OrderString.Free;
    Order.Free;
    //FClocks.Widget.Reorder;
  end;
end;


end.
