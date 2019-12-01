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
unit mainformwidget;

{$mode objfpc}{$H+}
//{$INTERFACES CORBA}
interface

uses
  Classes, SysUtils, Dialogs, Graphics, IntfGraphics, Forms, LCLIntf,
  LCLType, dateutils, BGRABitmap,
  BGRABitmapTypes, FPimage, observers, main, timeralertform, settings,
  clockswidget, optionsform;

const
  TRAY_PROGRESS_ICON_COUNT = 24;

  TRAY_OUTLINE_COLOUR = $3333f2;//$5A9E60;
  TRAY_BG_COLOUR = $c6c6f5;//$DAEADB;
  TRAY_CENTRE_COLOUR = TRAY_BG_COLOUR;
  PROGRESS_COLOUR = $7a2a41;//1A1AB0;

  PROGRESS_COMPLETED = 2.0;

  TRAY_OUTLINE_INSET = 2;
  TRAY_CENTRE_INNER_RADIUS = 2;
  TRAY_CENTRE_OUTER_RADIUS = 4;
  TRAY_BASE_WIDTH = 16;

  RAD_MULTIPLIER = 16;

  APP_ICON_SIZE = 256;

  LAST_TRAY_ICON_DEFAULT = -1;



type

  { TMainFormWidget }

  TMainFormWidget = class(TInterfacedObject, ITimerObserver)
  private

    FForm: TMainForm;
    // Array of bitmaps to hold the tray icons to show progress
    FTrayProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FAppProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FTrayStoppedBitmap, FAppStoppedBitmap: TIcon;
    FLastTrayIconIndex: integer;
    //FTrayEnabledIcon: TIcon;
    function GetClocks: TClocksWidget;
    procedure DrawBaseIconBackground(Bmp: TBGRABitmap);
    procedure DrawBaseIconForeground(Bmp: TBGRABitmap);
    procedure SetDeleteEnabled(AValue: boolean);
    procedure SetForm(AValue: TMainForm);
    procedure SetMoveDownEnabled(AValue: boolean);
    procedure SetMoveUpEnabled(AValue: boolean);
    procedure FormShow(Sender: TObject);
    procedure UpdateAlertFormSettings;
  public
    OnNewTimer: TNotifyEvent;
    OnNewAlarm: TNotifyEvent;
    OnClockDelete: TNotifyEvent;
    OnClockMoveUp: TNotifyEvent;
    OnClockMoveDown: TNotifyEvent;
    OnEXport: TNotifyEvent;
    constructor Create();
    destructor Destroy; override;

    procedure NewTimerAdded(Sender: TObject);
    procedure NewAlarmAdded(Sender: TObject);
    procedure ClockDeleted(Sender: TObject);
    procedure ClockMovedUp(Sender: TObject);
    procedure ClockMovedDown(Sender: TObject);
    procedure TimerFinished(Id: integer);
    procedure ProgressUpdate(Progress: single);
    procedure OptionsEdit(Sender: TObject);
    procedure ExportClicked(Sender: TObject);
    procedure OptionsClose(Sender: TObject; var Action: TCloseAction);
    function GetExportFileName: string;
    property Form: TMainForm read FForm write SetForm;
    property Clocks: TClocksWidget read GetClocks;
    property DeleteEnabled: boolean write SetDeleteEnabled;
    property MoveUpEnabled: boolean write SetMoveUpEnabled;
    property MoveDownEnabled: boolean write SetMoveDownEnabled;



  end;

implementation

{ TMainFormWidget }

function TMainFormWidget.GetClocks: TClocksWidget;
begin
  Result := FForm.Clocks;
end;

procedure TMainFormWidget.DrawBaseIconBackground(Bmp: TBGRABitmap);
var
  TrayInset{, InnerRadius, OuterRadius}: integer;
  Size: integer;
begin
  Size := Bmp.Width;
  TrayInset := (TRAY_OUTLINE_INSET * Size) div TRAY_BASE_WIDTH;
  //InnerRadius := (TRAY_CENTRE_INNER_RADIUS * Size) div TRAY_BASE_WIDTH;
  //InnerRadius := (TRAY_CENTRE_Inner_RADIUS * Size) div TRAY_BASE_WIDTH;
  //Bmp := TBGRABitmap.Create(Size, Size, BGRAPixelTransparent);
  //Bmp.SetSize(Size, Size);
  with Bmp.CanvasBGRA do
  begin
    Brush.Color := TRAY_OUTLINE_COLOUR;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Ellipse(0, 0, Size, Size);
    Brush.Color := TRAY_BG_COLOUR;
    Pen.Color := TRAY_BG_COLOUR;
    Ellipse(TrayInset, TrayInset, Size - TrayInset, Size - TrayInset);
    {Brush.Color := TRAY_OUTLINE_COLOUR;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Ellipse((Size div 2) - InnerRadius, (Size div 2) - InnerRadius,
      (Size div 2) + InnerRadius, (Size div 2) + InnerRadius);
    Brush.Color := TRAY_BG_COLOUR;
    Pen.Color := TRAY_BG_COLOUR;
    Ellipse((Size div 2) - InnerRadius, (Size div 2) - InnerRadius,
      (Size div 2) + InnerRadius, (Size div 2) + InnerRadius); }
  end;

end;

procedure TMainFormWidget.DrawBaseIconForeground(Bmp: TBGRABitmap);
var
  InnerRadius, OuterRadius: integer;
  Size: integer;
begin
  Size := Bmp.Width;
  InnerRadius := (TRAY_CENTRE_INNER_RADIUS * Size) div TRAY_BASE_WIDTH;
  OuterRadius := (TRAY_CENTRE_OUTER_RADIUS * Size) div TRAY_BASE_WIDTH;
  with Bmp.CanvasBGRA do
  begin
    Brush.Color := TRAY_OUTLINE_COLOUR;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Ellipse((Size div 2) - OuterRadius, (Size div 2) - OuterRadius,
      (Size div 2) + OuterRadius, (Size div 2) + OuterRadius);
    Brush.Color := TRAY_CENTRE_COLOUR;
    Pen.Color := TRAY_CENTRE_COLOUR;
    Ellipse((Size div 2) - InnerRadius, (Size div 2) - InnerRadius,
      (Size div 2) + InnerRadius, (Size div 2) + InnerRadius);

    Brush.Color := clWhite;
    Brush.Opacity := 80;
    Pen.Color := clWhite;
    Pen.Opacity := 80;
    Pie(0, 0, Bmp.Width, Bmp.Width, 90 * RAD_MULTIPLIER, -(180 * RAD_MULTIPLIER));

    Brush.Color := TRAY_OUTLINE_COLOUR;
    Brush.Opacity := 50;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Pen.Opacity := 50;
    Pie(0, 0, Bmp.Width, Bmp.Width, -(60 * RAD_MULTIPLIER), (120 * RAD_MULTIPLIER));
  end;
end;

procedure TMainFormWidget.SetDeleteEnabled(AValue: boolean);
begin
  FForm.sbDelete.Enabled := AValue;
end;

procedure TMainFormWidget.SetForm(AValue: TMainForm);
var
  Count: integer;
  bmp: TBGRABitmap;
  TrayIconSize: integer;
  InSet: integer;
begin
  if FForm = AValue then
    Exit;
  FForm := AValue;
  FForm.aiNewTimer.OnExecute := @NewTimerAdded;
  FForm.aiNewAlarm.OnExecute := @NewAlarmAdded;
  FForm.aiOptions.OnExecute := @OptionsEdit;
  FForm.sbDelete.OnClick := @ClockDeleted;
  FForm.sbMoveClockUp.OnClick := @ClockMovedUp;
  FForm.sbMoveClockDown.OnClick := @ClockMovedDown;
  FForm.aiExport.OnExecute:=@ExportClicked;
  FForm.OnShow := @FormShow;


  TrayIconSize := TRAY_BASE_WIDTH;
  TrayIconSize := GetSystemMetrics(SM_CXSMICON);

  bmp := TBGRABitmap.Create(TrayIconSize, TrayIconSize, BGRAPixelTransparent);

  DrawBaseIconBackground(bmp);
  DrawBaseIconForeground(bmp);

  FTrayStoppedBitmap := TIcon.Create;
  FTrayStoppedBitmap.Assign(bmp.Bitmap);

  bmp.Free;


  bmp := TBGRABitmap.Create(APP_ICON_SIZE, APP_ICON_SIZE, BGRAPixelTransparent);
  DrawBaseIconBackground(bmp);
  DrawBaseIconForeground(bmp);

  FAppStoppedBitmap := TIcon.Create;
  FAppStoppedBitmap.Assign(bmp.Bitmap);

  bmp.Free;

  ProgressUpdate(PROGRESS_COMPLETED);

  InSet := 2;

  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin

    bmp :=
      TBGRABitmap.Create(TrayIconSize, TrayIconSize, BGRAPixelTransparent);

    DrawBaseIconBackground(bmp);
    with bmp do
    begin

      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;

      CanvasBGRA.Pie(InSet, InSet, (TRAY_BASE_WIDTH - InSet),
        (TRAY_BASE_WIDTH - InSet), 90 * RAD_MULTIPLIER,
        -(15 * RAD_MULTIPLIER * (Count - 1)));
    end;
    DrawBaseIconForeground(bmp);
    FTrayProgressIcons[Count] := TIcon.Create;

    FTrayProgressIcons[Count].Assign(bmp.Bitmap);

    bmp.Free;

    bmp :=
      TBGRABitmap.Create(APP_ICON_SIZE, APP_ICON_SIZE, BGRAPixelTransparent);
    DrawBaseIconBackground(bmp);
    with bmp do
    begin

      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;

      CanvasBGRA.Pie((Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH,
        (Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH,
        (APP_ICON_SIZE - ((Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH)),
        (APP_ICON_SIZE - ((Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH)),
        90 * RAD_MULTIPLIER, -(15 * RAD_MULTIPLIER * (Count - 1)));
    end;
    DrawBaseIconForeground(bmp);
    FAppProgressIcons[Count] := TIcon.Create;
    FAppProgressIcons[Count].Assign(bmp.Bitmap);

    bmp.Free;
  end;

end;

procedure TMainFormWidget.SetMoveDownEnabled(AValue: boolean);
begin
  FForm.sbMoveClockDown.Enabled := AValue;
end;

procedure TMainFormWidget.SetMoveUpEnabled(AValue: boolean);
begin
  FForm.sbMoveClockUp.Enabled := Avalue;
end;

procedure TMainFormWidget.FormShow(Sender: TObject);
begin
  frmOptions.OnClose := @OptionsClose;
  UpdateAlertFormSettings;

end;

procedure TMainFormWidget.UpdateAlertFormSettings;
begin
  with GlobalUserConfig do
  begin
    frmTimerAlert.Color := ModalBackgroundColour;
    frmTimerAlert.stxMessage.Font.Color := ModalCaptionColour;
    frmTimerAlert.stxAdditional.Font.Color := ModalSubtextColour;
  end;
end;

constructor TMainFormWidget.Create;
begin

  OnNewTimer := nil;
  OnEXport := nil;
  FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;
end;

destructor TMainFormWidget.Destroy;
var
  Count: integer;
begin
  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin
    FTrayProgressIcons[Count].Free;
    FAppProgressIcons[Count].Free;
  end;
  FTrayStoppedBitmap.Free;
  FAppStoppedBitmap.Free;

  inherited Destroy;
end;


procedure TMainFormWidget.NewTimerAdded(Sender: TObject);
begin

  if OnNewTimer <> nil then
    OnNewTimer(Sender);
end;

procedure TMainFormWidget.NewAlarmAdded(Sender: TObject);
begin
  if OnNewAlarm <> nil then
    OnNewAlarm(Sender)
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNewAlarm was found to be Nil');
end;

procedure TMainFormWidget.ClockDeleted(Sender: TObject);
begin
  //ShowMessage('Haha');
  if OnClockDelete <> nil then
  begin
    OnClockDelete(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnClockDelete was found to be Nil');
end;

procedure TMainFormWidget.ClockMovedUp(Sender: TObject);
begin
  if OnClockMoveUp <> nil then
  begin
    OnClockMoveUp(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnClockMoveUp was found to be Nil');
end;

procedure TMainFormWidget.ClockMovedDown(Sender: TObject);
begin
  if OnClockMoveDown <> nil then
  begin
    OnClockMoveDown(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnClockMoveDown was found to be Nil');
end;

procedure TMainFormWidget.TimerFinished(Id: integer);
var
  Hours: word;
  Minutes: word;
  Seconds: word;
  Widget: TTimerClockWidget;
  Duration: TDateTime;
begin
  //DecodeTime(Duration, Hours, Minutes, Seconds, Millis);

  Widget := Clocks.GetClock(Id);
  Duration := Widget.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  if GlobalUserConfig.ShowTrayAlert then
  begin
    FForm.tiMain.BalloonHint :=
      Widget.Caption + ' completed. (' + Format('%.2d', [Hours]) +
      ':' + Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    FForm.tiMain.ShowBalloonHint;
  end;

  if GlobalUserConfig.ShowModalAlert then
  begin
    frmTimerAlert.stxAdditional.Caption :=
      Widget.Caption + ' completed. (' + Format('%.2d', [Hours]) +
      ':' + Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    if not frmTimerAlert.Showing then
      frmTimerAlert.ShowModal;
  end;

end;

procedure TMainFormWidget.ProgressUpdate(Progress: single);
var
  //Bmp: TBitmap;
  Index: integer;
begin
  if (Progress > 1.99) and (Progress < 2.01) then
  begin
    FForm.tiMain.Icon.Assign(FTrayStoppedBitmap);
    FForm.Icon.Assign(FAppStoppedBitmap);
    Application.Icon.Assign(FAppStoppedBitmap);
    FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;

  end
  else
  begin
    // TODO: Which is correct? Round ro Trunc?
    Index := Trunc(Progress * 24.0);
    if Index = 24 then
      Index := 23;
    Assert((Index >= 0) and (Index < TRAY_PROGRESS_ICON_COUNT));
    if FLastTrayIconIndex <> Index then
    begin
      FForm.tiMain.Icon.Assign(FTrayProgressIcons[Index + 1]);
      //FForm.tiMain.Icon.Handle:=FTrayProgressIcons[Index + 1].Handle;
      FForm.Icon.Assign(FAppProgressIcons[Index + 1]);
      //FForm.Icon.Handle := FTrayProgressIcons[Index + 1].Handle;
      Application.Icon.Assign(FAppProgressIcons[Index + 1]);
      FLastTrayIconIndex := Index;
    end;
  end;

end;

procedure TMainFormWidget.OptionsEdit(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TMainFormWidget.ExportClicked(Sender: TObject);
begin
  if OnExport <> nil then
  begin
    OnExport(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnExport was found to be Nil');
end;

procedure TMainFormWidget.OptionsClose(Sender: TObject; var Action: TCloseAction);
begin

  {Once the options form is closed, tool buttons have to be set according
  to the changed options}
  with GlobalUserConfig do
  begin
    if FForm.tbShowModalAlert.Down <> ShowModalAlert then
    begin
      FForm.tbShowModalAlert.Down := ShowModalAlert;
      FForm.tbShowModalAlertClick(Self);
    end;

    if FForm.tbShowTrayAlert.Down <> ShowTrayAlert then
    begin
      FForm.tbShowTrayAlert.Down := ShowTrayAlert;
      FForm.tbShowTrayAlertClick(Self);
    end;

    if FForm.tbProgressAuto.Down <> AutoProgress then
    begin
      FForm.tbProgressAuto.Down := AutoProgress;
      FForm.tbProgressAutoClick(Self);
    end;
  end;

  UpdateAlertFormSettings;

end;

function TMainFormWidget.GetExportFileName: string;
begin
  if FForm.sdgExport.Execute then
    Result := FForm.sdgExport.FileName
  else
    Result := '';
end;


end.
