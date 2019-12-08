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
unit timerframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, EditBtn, Dialogs, dateutils, settings, observers;

const
  TIMER_IMG_GREY_TIMER: integer = 0;
  TIMER_IMG_COLOUR_TIMER: integer = 1;
  TIMER_IMG_NOTIFY_YES: integer = 2;
  TIMER_IMG_NOTIFY_NO: integer = 3;

  CLOCK_HEIGHT = 38;

type

  { TfraTimer }

  TfraTimer = class(TFrame)
    cbSelect: TCheckBox;
    dtpSet: TDateTimePicker;
    edtTitle: TEdit;
    ilTimer: TImageList;
    imgTimer: TImage;
    lblCountdown: TLabel;
    sbPlay: TSpeedButton;
    sbPause: TSpeedButton;
    sbStop: TSpeedButton;
    sbNotify: TSpeedButton;
    procedure dtpSetChange(Sender: TObject);
    procedure imgTimerClick(Sender: TObject);
    procedure sbNotifyClick(Sender: TObject);
    procedure UpdateNotifyButton;
  private
    { private declarations }
    FId: longword;
    //FObservers: TTimerObserverList;
    procedure SetId(AValue: longword);
    function GetCaption: string;
    function GetCounter: string;
    function GetDuration: TDateTime;
    function GetDurationEnabled: boolean;
    function GetImageGreyed: boolean;
    function GetNotifyEnabled: boolean;
    function GetPauseButtonEnabled: boolean;
    function GetPlayButtonEnabled: boolean;
    function GetSelected: boolean;
    function GetStopButtonEnabled: boolean;
    procedure SetCaption(AValue: string);
    //procedure SetCounter(AValue: string);
    procedure SetDuration(AValue: TDateTime);
    procedure SetDurationEnabled(AValue: boolean);
    //procedure SetId(AValue: longword);
    procedure SetImageGreyed(AValue: boolean);
    procedure SetNotifyEnabled(AValue: boolean);
    procedure SetPauseButtonEnabled(AValue: boolean);
    procedure SetPlayButtonEnabled(AValue: boolean);
    procedure SetStopButtonEnabled(AValue: boolean);
    function GetId: longword;
    {function GetTop: integer;
    procedure SetTop(AValue: integer);
    function GetHeight: integer;
    procedure SetHeight(AValue: integer);
    function GetWidth: integer;
    procedure SetWidth(AValue: integer);   }
  public
    { public declarations }
    OnNotifyClick: TNotifyEvent;
    OnPlay: TNotifyEvent;
    OnStop: TNotifyEvent;
    OnPause: TNotifyEvent;
    OnNotify: TNotifyEvent;
    OnSelect: TNotifyEvent;
    procedure PlayClicked(Sender: TObject);
    procedure Stopclicked(Sender: TObject);
    procedure PauseClicked(Sender: TObject);
    procedure NotifyClicked(Sender: TObject);
    procedure ClockSelected(Sender: TObject);
    procedure Hide;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Id: longword read FId write SetId;
    procedure SetCounter(AValue: string);
    procedure CheckForZeroTime;
    property PlayButtonEnabled: boolean read GetPlayButtonEnabled
      write SetPlayButtonEnabled;
    property PauseButtonEnabled: boolean read GetPauseButtonEnabled
      write SetPauseButtonEnabled;
    property StopButtonEnabled: boolean read GetStopButtonEnabled
      write SetStopButtonEnabled;
    property Counter: string read GetCounter write SetCounter;
    property Duration: TDateTime read GetDuration write SetDuration;
    property DurationEnabled: boolean read GetDurationEnabled write SetDurationEnabled;
    property Caption: string read GetCaption write SetCaption;
    property ImageGreyed: boolean read GetImageGreyed write SetImageGreyed;
    property NotifyEnabled: boolean read GetNotifyEnabled write SetNotifyEnabled;
    property Selected: boolean read GetSelected;
    //property Id: longword read GetId write SetId;
    //property Top: integer read GetTop write SetTop;
    //property Height: integer read GetHeight write SetHeight;
    //property Width: integer read GetWidth write SetWidth;
  end;


implementation

{$R *.lfm}

{ TfraTimer }


procedure TfraTimer.dtpSetChange(Sender: TObject);
begin
  CheckForZeroTime;
end;

procedure TfraTimer.imgTimerClick(Sender: TObject);
begin

end;

procedure TfraTimer.sbNotifyClick(Sender: TObject);
begin
  UpdateNotifyButton;
  if OnNotifyClick <> nil then
    OnNotifyClick(Self)
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotifyClick was found to be Nil');
end;

procedure TfraTimer.UpdateNotifyButton;
begin
  if sbNotify.Down then
    ilTimer.GetBitmap(TIMER_IMG_NOTIFY_YES, sbNotify.Glyph)
  else
    ilTimer.GetBitmap(TIMER_IMG_NOTIFY_NO, sbNotify.Glyph);
end;


procedure TfraTimer.SetId(AValue: longword);
begin
  Assert(AValue > 0);
  if FId > 0 then
    Exit;
  if FId = AValue then
    Exit;
  FId := AValue;
  Name := Name + IntToStr(AValue);
  //Id := IdNew;
end;

function TfraTimer.GetCaption: string;
begin
  Result := edtTitle.Text;
end;

function TfraTimer.GetCounter: string;
begin
  Result := lblCountdown.Caption;
end;

function TfraTimer.GetDuration: TDateTime;
begin
  Result := dtpSet.Time;
end;

function TfraTimer.GetDurationEnabled: boolean;
begin
  Result := dtpSet.Enabled;
end;

function TfraTimer.GetImageGreyed: boolean;
begin
  Result := imgTimer.Visible;
end;

function TfraTimer.GetNotifyEnabled: boolean;
begin
  Result := sbNotify.Down;
end;

function TfraTimer.GetPauseButtonEnabled: boolean;
begin
  Result := sbPause.Enabled;
end;

function TfraTimer.GetPlayButtonEnabled: boolean;
begin
  Result := sbPlay.Enabled;
end;

function TfraTimer.GetSelected: boolean;
begin
  Result := cbSelect.Checked;
end;

function TfraTimer.GetStopButtonEnabled: boolean;
begin
  Result := sbStop.Enabled;
end;

procedure TfraTimer.SetCaption(AValue: string);
begin
  edtTitle.Text := AValue;
end;

procedure TfraTimer.SetDuration(AValue: TDateTime);
begin
  dtpSet.Time := AValue;
  CheckForZeroTime;
end;

procedure TfraTimer.SetDurationEnabled(AValue: boolean);
begin
  dtpSet.Enabled := AValue;
end;

procedure TfraTimer.SetImageGreyed(AValue: boolean);
begin
  if AValue then
    ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, imgTimer.Picture.Bitmap)
  else
    ilTimer.GetBitmap(TIMER_IMG_COLOUR_TIMER, imgTimer.Picture.Bitmap);
  //FFrame.imgTimer.Picture.Bitmap;
end;

procedure TfraTimer.SetNotifyEnabled(AValue: boolean);
begin
  sbNotify.Down := AValue;
  UpdateNotifyButton;
end;

procedure TfraTimer.SetPauseButtonEnabled(AValue: boolean);
begin
  sbPause.Enabled := AValue;
end;

procedure TfraTimer.SetPlayButtonEnabled(AValue: boolean);
begin
  sbPlay.Enabled := AValue;
end;

procedure TfraTimer.SetStopButtonEnabled(AValue: boolean);
begin
  sbStop.Enabled := AValue;
end;

function TfraTimer.GetId: longword;
begin
  Result := FId;
end;

procedure TfraTimer.PlayClicked(Sender: TObject);
begin
  //ShowMessage('In Widget');
  if OnPlay <> nil then
  begin
    OnPlay(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnPlay was found to be Nil');
end;

procedure TfraTimer.Stopclicked(Sender: TObject);
begin
  if OnStop <> nil then
  begin
    OnStop(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnStop was found to be Nil');
end;

procedure TfraTimer.PauseClicked(Sender: TObject);
begin
  if OnPause <> nil then
  begin
    OnPause(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnPause was found to be Nil');
end;

procedure TfraTimer.NotifyClicked(Sender: TObject);
begin
  if OnNotify <> nil then
  begin
    OnNotify(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotify was found to be Nil');
end;

procedure TfraTimer.ClockSelected(Sender: TObject);
begin
  //ShowMessage('TTimerClockWidget.Selected');
  if OnSelect <> nil then
  begin
    OnSelect(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnSelect was found to be Nil');
end;

procedure TfraTimer.Hide;
begin
  Top := Top + Height;
end;

{function TfraTimer.GetTop: integer;
begin
  Result := FFrame.Top;
end;

procedure TfraTimer.SetTop(AValue: integer);
begin
  FFrame.Top := AValue;
end;

function TfraTimer.GetHeight: integer;
begin
  Result := FFrame.Height;
end;

procedure TfraTimer.SetHeight(AValue: integer);
begin
  FFrame.Height := AValue;
end;

function TfraTimer.GetWidth: integer;
begin
  Result := FFrame.Width;
end;

procedure TfraTimer.SetWidth(AValue: integer);
begin
  FFrame.Width := AValue;
end; }

constructor TfraTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FId := 0;
  //teSet.Time := EncodeTime(0, GlobalDefault.TimerInitMins, 0, 0);
  with GlobalUserConfig do
  begin
    dtpSet.Time := EncodeTime(DefaultTimerHours, DefaultTimerMins,
      DefaultTimerSecs, 0);
    edtTitle.Text := DefaultTimerTitle;
  end;

  //  sbNotify.Glyph;
  ilTimer.GetBitmap(TIMER_IMG_NOTIFY_NO, sbNotify.Glyph);
  //teSet.

  OnPlay := nil;

  Parent := TWinControl(AOwner);
  //Name := Name + IntToStr(IdNew);
  //Id := IdNew;
  sbPlay.OnClick := @PlayClicked;
  sbStop.OnClick := @StopClicked;
  sbPause.OnClick := @PauseClicked;
  OnNotifyClick := @NotifyClicked;
  cbSelect.OnChange := @ClockSelected;

  ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, imgTimer.Picture.Bitmap);

end;

destructor TfraTimer.Destroy;
begin
  Parent := Nil;
  inherited Destroy;
end;

procedure TfraTimer.SetCounter(AValue: string);
begin
  lblCountdown.Caption := AValue;
end;

procedure TfraTimer.CheckForZeroTime;
var
  TempDuration: TDateTime;
  Hours: word;
  Minutes: word;
  Seconds: word;
begin
  TempDuration := dtpSet.Time;
  Hours := HourOf(TempDuration);
  Minutes := MinuteOf(TempDuration);
  Seconds := SecondOf(TempDuration);
  // If Hours, Minutes, Seconds, all are zero then disable play button
  sbPlay.Enabled := not ((Hours = 0) and (Minutes = 0) and (Seconds = 0));
end;

end.
