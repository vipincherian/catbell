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
unit clockswidget;

{$mode objfpc}{$H+}

interface
implementation
begin

end.
{*
uses
  Classes, SysUtils, ExtCtrls, Dialogs, Forms, Graphics, LCLIntf,
  LCLType, fgl, timerframe;

const
  CLOCK_HEIGHT = 38;

type


  { TTimerClockWidget }
  TTimerClockWidget = class(TObject)
  private
    FFrame: TfraTimer;
    FId: longword;
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
    procedure SetCounter(AValue: string);
    procedure SetDuration(AValue: TDateTime);
    procedure SetDurationEnabled(AValue: boolean);
    procedure SetId(AValue: longword);
    procedure SetImageGreyed(AValue: boolean);
    procedure SetNotifyEnabled(AValue: boolean);
    procedure SetPauseButtonEnabled(AValue: boolean);
    procedure SetPlayButtonEnabled(AValue: boolean);
    procedure SetStopButtonEnabled(AValue: boolean);
    function GetId: longword;
    function GetTop: integer;
    procedure SetTop(AValue: integer);
    function GetHeight: integer;
    procedure SetHeight(AValue: integer);
    function GetWidth: integer;
    procedure SetWidth(AValue: integer);
  public
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
    constructor Create(OwnerBox: TScrollbox; IdNew: longword);
    destructor Destroy; override;
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
    property Id: longword read GetId write SetId;
    property Top: integer read GetTop write SetTop;
    property Height: integer read GetHeight write SetHeight;
    property Width: integer read GetWidth write SetWidth;

  end;


  TListTimerClockWidgets = specialize TFPGList<TTimerClockWidget>;

  //TClockWidgetList = specialize TFPGMap<longword, TTimerClockWidget>;

  //TIdList = specialize TFPGList<longword>;    *}
  {*
  { TClocksWidget }

  TClocksWidget = class(TObject)
  private
    FScrollBox: TScrollBox;
    FClockWidgets: TClockWidgetList;
    FOrder: TIdList;
    function GetCanselectedMoveDown: boolean;
    function GetCanSelectedMoveUp: boolean;
    procedure SetScrollBox(AValue: TScrollBox);
    procedure Reorder;
  public
    constructor Create();
    destructor Destroy; override;
    function AddTimer(IdNew: longword): TTimerClockWidget;
    procedure RemoveTimer(IdNew: longword);
    procedure MoveSelectedClocksUp;
    procedure MoveSelectedClocksDown;
    procedure GetOrder(AValue: TIdList);
    procedure SetOrder(AValue: TIdList);
    function GetClock(Id: longword): TTimerClockWidget;
    property ScrollBox: TScrollBox read FScrollBox write SetScrollBox;
    property CanSelectedMoveUp: boolean read GetCanSelectedMoveUp;
    property CanSelectedMovDown: boolean read GetCanselectedMoveDown;

  end;




implementation


{ TTimerClockWidget }

function TTimerClockWidget.GetPlayButtonEnabled: boolean;
begin
  Result := FFrame.sbPlay.Enabled;
end;


function TTimerClockWidget.GetPauseButtonEnabled: boolean;
begin
  Result := FFrame.sbPause.Enabled;
end;

function TTimerClockWidget.GetCounter: string;
begin
  Result := FFrame.lblCountdown.Caption;
end;

function TTimerClockWidget.GetCaption: string;
begin
  Result := FFrame.edtTitle.Text;
end;

function TTimerClockWidget.GetDuration: TDateTime;
begin
  Result := FFrame.dtpSet.Time;
end;

function TTimerClockWidget.GetDurationEnabled: boolean;
begin
  Result := FFrame.dtpSet.Enabled;
end;

function TTimerClockWidget.GetImageGreyed: boolean;
begin
  Result := FFrame.imgTimer.Visible;
end;

function TTimerClockWidget.GetNotifyEnabled: boolean;
begin
  Result := FFrame.sbNotify.Down;
end;

function TTimerClockWidget.GetSelected: boolean;
begin
  Result := FFrame.cbSelect.Checked;
end;

function TTimerClockWidget.GetStopButtonEnabled: boolean;
begin
  Result := FFrame.sbStop.Enabled;
end;

procedure TTimerClockWidget.SetCaption(AValue: string);
begin
  FFrame.edtTitle.Text := AValue;
end;

procedure TTimerClockWidget.SetCounter(AValue: string);
begin
  FFrame.lblCountdown.Caption := AValue;
end;

procedure TTimerClockWidget.SetDuration(AValue: TDateTime);
begin
  FFrame.dtpSet.Time := AValue;
  FFrame.CheckForZeroTime;
end;

procedure TTimerClockWidget.SetDurationEnabled(AValue: boolean);
begin
  FFrame.dtpSet.Enabled := AValue;
end;

procedure TTimerClockWidget.SetId(AValue: longword);
begin
  Fid := AValue;
end;

procedure TTimerClockWidget.SetImageGreyed(AValue: boolean);
begin

  if AValue then
    FFrame.ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, FFrame.imgTimer.Picture.Bitmap)
  else
    FFrame.ilTimer.GetBitmap(TIMER_IMG_COLOUR_TIMER, FFrame.imgTimer.Picture.Bitmap);
  //FFrame.imgTimer.Picture.Bitmap;
end;

procedure TTimerClockWidget.SetNotifyEnabled(AValue: boolean);
begin
  FFrame.sbNotify.Down := AValue;
  FFrame.UpdateNotifyButton;
end;

procedure TTimerClockWidget.SetPauseButtonEnabled(AValue: boolean);
begin
  FFrame.sbPause.Enabled := AValue;
end;

procedure TTimerClockWidget.SetPlayButtonEnabled(AValue: boolean);
begin
  FFrame.sbPlay.Enabled := AValue;
end;

procedure TTimerClockWidget.SetStopButtonEnabled(AValue: boolean);
begin
  FFrame.sbStop.Enabled := AValue;
end;

function TTimerClockWidget.GetId: longword;
begin
  Result := FId;
end;

function TTimerClockWidget.GetTop: integer;
begin
  Result := FFrame.Top;
end;

procedure TTimerClockWidget.SetTop(AValue: integer);
begin
  FFrame.Top := AValue;
end;

function TTimerClockWidget.GetHeight: integer;
begin
  Result := FFrame.Height;
end;

procedure TTimerClockWidget.SetHeight(AValue: integer);
begin
  FFrame.Height := AValue;
end;

function TTimerClockWidget.GetWidth: integer;
begin
  Result := FFrame.Width;
end;

procedure TTimerClockWidget.SetWidth(AValue: integer);
begin
  FFrame.Width := AValue;
end;

procedure TTimerClockWidget.PlayClicked(Sender: TObject);
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

procedure TTimerClockWidget.Stopclicked(Sender: TObject);
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

procedure TTimerClockWidget.PauseClicked(Sender: TObject);
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

procedure TTimerClockWidget.NotifyClicked(Sender: TObject);
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

procedure TTimerClockWidget.ClockSelected(Sender: TObject);
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

procedure TTimerClockWidget.Hide;
begin
  //FFrame.Hide;
  //FFrame.Show;
  FFrame.Top := FFrame.Top + FFrame.Height;
end;

constructor TTimerClockWidget.Create(OwnerBox: TScrollbox; IdNew: longword);
begin
  inherited Create;

  OnPlay := nil;
  FId := IdNew;

  FFrame := TfraTimer.Create(OwnerBox.Owner);
  with FFrame do
  begin
    Parent := OwnerBox;
    Name := Name + IntToStr(IdNew);
    Id := IdNew;
    sbPlay.OnClick := @PlayClicked;
    sbStop.OnClick := @StopClicked;
    sbPause.OnClick := @PauseClicked;
    FFrame.OnNotifyClick := @NotifyClicked;
    cbSelect.OnChange := @ClockSelected;
  end;
  FFrame.ilTimer.GetBitmap(TIMER_IMG_GREY_TIMER, FFrame.imgTimer.Picture.Bitmap);
  //FFrame.imgTimer.Refresh;
  //FKind := 'Timer';
  //FFrame.Left := 100;
end;

destructor TTimerClockWidget.Destroy;
begin
  FFrame.Parent := nil;
  FFrame.Free;
  inherited Destroy;
end;
*}
{*
{ TClocksWidget }


procedure TClocksWidget.SetScrollBox(AValue: TScrollBox);
begin
  if AValue = nil then
    Exit;
  if FScrollBox = AValue then
    Exit;
  FScrollBox := AValue;
  //OnTimerStart:=Nil;
end;

function TClocksWidget.GetCanSelectedMoveUp: boolean;
var
  Id: longword;
  //Count: integer;
  EncounteredUnselected: boolean;
begin
  //EncounteredSelected := False;
  EncounteredUnselected := False;
  for Id in FOrder do
  begin
    if FClockWidgets.KeyData[Id].Selected then
    begin
      if EncounteredUnselected then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      if not EncounteredUnselected then
        EncounteredUnselected := True;
    end;
  end;
  Result := False;
end;

function TClocksWidget.GetCanselectedMoveDown: boolean;
var
  Id: longword;
  //Count: integer;
  EncounteredSelected: boolean;
begin
  EncounteredSelected := False;
  //EncounteredUnselected := False;
  for Id in FOrder do
  begin
    if not FClockWidgets.KeyData[Id].Selected then
    begin
      if EncounteredSelected then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      if not EncounteredSelected then
        EncounteredSelected := True;
    end;
  end;
  Result := False;
end;

constructor TClocksWidget.Create;
begin

  FClockWidgets := TClockWidgetList.Create;
  FOrder := TIdList.Create;

end;

destructor TClocksWidget.Destroy;
var
  I: integer;
begin
  FOrder.Free;
  for I := 0 to FClockWidgets.Count - 1 do
  begin
    //TODO: Is there a memory leak here?
    FClockWidgets.Data[i].Free;
  end;

  FClockWidgets.Free;
  inherited Destroy;
end;


function TClocksWidget.AddTimer(IdNew: longword): TTimerClockWidget;
var
  NewTimerWidget: TTimerClockWidget;
begin
  NewTimerWidget := TTimerClockWidget.Create(FScrollBox, IdNew);
  FClockWidgets.Add(IdNew, NewTimerWidget);
  FOrder.Insert(0, IdNew);
  Reorder;
  Result := NewTimerWidget;
end;

procedure TClocksWidget.RemoveTimer(IdNew: longword);
var
  RemovedTimer: TTimerClockWidget;
  Index: integer;
begin
  Index := FClockWidgets.IndexOf(IdNew);
  RemovedTimer := TTimerClockWidget(FClockWidgets.Data[Index]);
  FClockWidgets.Remove(IdNew);
  FOrder.Remove(IdNew);
  RemovedTimer.Free;
  Reorder;
end;

procedure TClocksWidget.Reorder;
var
  Id: longword;
  //Index: integer;
  TimerWidget: TTimerClockWidget;
  Filled: integer;
begin
  //Exit;
  Filled := 0;
  //FScrollBox.AutoScroll:=False;
  //FScrollBox.Visible:=False;
  FScrollBox.Height := FOrder.Count * CLOCK_HEIGHT;
  for Id in FOrder do
  begin
    //Index := FClockWidgets.IndexOf(Id);
    TimerWidget := FClockWidgets.KeyData[Id];
    //FScrollBox.Height:=Filled + Clock.Height;
    //FScrollBox.VertScrollBar.Range:=Filled + Clock.Height;
    TimerWidget.Top := Filled;
    // + FScrollBox.VertScrollBar.Size - FScrollBox.VertScrollBar.Position;
    if FScrollBox.VertScrollBar.IsScrollBarVisible then
      TimerWidget.Width := FSCrollBox.Width - GetSystemMetrics(SM_CYVSCROLL)
    else
      TimerWidget.Width := FSCrollBox.Width;
    //Clock.He;
    Inc(Filled, TimerWidget.Height);
    //if Clock.Kind = 'Timer' then
    //begin
    //TimerWidget := TTimerClockWidget(Clock);

    TimerWidget.FFrame.lblCountdown.Hint := IntToStr(TimerWidget.FFrame.Top);
    //end;

  end;

  //FScrollBox.AutoScroll:=True;
  //FScrollBox.VertScrollBar.Range:=(FOrder.Count) * CLOCK_HEIGHT;

  //FScrollBox.VertScrollBar.Range:=Filled ;
  //FScrollBox.Refresh;
  //FScrollBox.Visible:=True;
  //FScrollBox.Repaint;
  //FScrollBox.ReAlign;

end;

procedure TClocksWidget.MoveSelectedClocksUp;
var
  Id: longword;
  Count: integer;
begin
  Count := 0;
  for Id in FOrder do
  begin
    if Count = 0 then
    begin
      Inc(Count);
      Continue;
    end;

    if FClockWidgets.KeyData[Id].Selected then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      if not (FClockWidgets.KeyData[FOrder.Items[Count - 1]].Selected and
        FClockWidgets.KeyData[FOrder.Items[Count]].Selected) then
        FOrder.Exchange(Count - 1, Count);
    end;
    Inc(Count);
  end;
  Reorder;
end;

procedure TClocksWidget.MoveSelectedClocksDown;
var
  Id: longword;
  Count: integer;
  First: boolean;
begin
  First := True;
  for Count := (FClockWidgets.Count - 1) downto 0 do
  begin
    if First then
    begin
      First := False;
      Continue;
    end;
    Id := FOrder.Items[Count];
    if FClockWidgets.KeyData[Id].Selected then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      if not (FClockWidgets.KeyData[FOrder.Items[Count + 1]].Selected and
        FClockWidgets.KeyData[FOrder.Items[Count]].Selected) then
        FOrder.Exchange(Count + 1, Count);
    end;

  end;
  Reorder;
end;

procedure TClocksWidget.GetOrder(AValue: TIdList);
var
  Id: integer;
begin
  AValue.Clear;
  for Id in FOrder do
  begin
    AValue.Add(Id);
  end;
end;

procedure TClocksWidget.SetOrder(AValue: TIdList);
var
  Id: integer;
begin
  FOrder.Clear;
  for Id in AValue do
  begin
    FOrder.Add(Id);
  end;
  ReOrder;
end;

function TClocksWidget.GetClock(Id: longword): TTimerClockWidget;
begin
  Result := FClockWidgets.KeyData[Id];
end;
*}


end.
