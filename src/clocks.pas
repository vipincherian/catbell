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
unit clocks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, ExtCtrls, fgl, dateutils, jsonConf, clockswidget,
  sequence, observers, settings, Graphics, LCLIntf,
  LCLType, timerframe;

const
  DEF_COUNTDOWN_CAPTION: string = '00:00:00';
  TIMER_PROGRESS_FINISHED: single = 2.0;

  TIMER_CONF_CLOCKS = 'clocks';
  TIMER_CONF_TIMERS = 'timers';
  TIMER_CONF_TITLE = 'timer_title';
  TIMER_CONF_TIME = 'time';
  TIMER_CONF_HOURS = 'hours';
  TIMER_CONF_MINUTES = 'minutes';
  TIMER_CONF_SECONDS = 'seconds';
  TIMER_CONF_NOTIFIER = 'notifier';
  //TIMER_CONF_ID = 'id';
  TIMER_CONF_COUNT = 'count';
  TIMER_CONF_ORDER = 'order';

type


  { TTimerClock }
  {*
  TTimerClock = class(TInterfacedObject, ITimerSubject)
  private
    FWidget: TfraTimer;
    FShortTimer: TTimer;
    FNotifier: boolean;

    FOrigTickDuration: longword;
    FStartTickCount: longword;
    FEndTickCount: longword;
    FPendingTickCount: longword;

    FPaused: boolean;
    FRunning: boolean;
    FObservers: TListTimerObservers;
    FId: integer;
    procedure SetId(AValue: integer);
    procedure SetNotifier(AValue: boolean);
    procedure SetWidget(AValue: TFraTimer);
    function GetSelected: boolean;

  public
    OnNotifyChange: TNotifyEvent;
    constructor Create();
    destructor Destroy; override;
    property Widget: TfraTimer read FWidget write SetWidget;
    property Id: integer read FId write SetId;
    procedure OnShortTimer(Sender: TObject);
    procedure Start(Sender: TObject);
    procedure Pause(Sender: TObject);
    procedure Stop(Sender: TObject);
    procedure NotifyChange(Sender: TObject);
    procedure Finish;
    procedure PublishProgress(Percent: single);
    procedure AddSubscription(aObserver: ITimerObserver);
    procedure RemoveSubscription(aObserver: ITimerObserver);
    property Selected: boolean read GetSelected;
    property Notifier: boolean read FNotifier write SetNotifier;
    property Running: boolean read FRunning;
  end; *}

  TClockList = specialize TFPGMap<longword, TfraTimer>;

  TClockWidgetList = specialize TFPGMap<longword, TfraTimer>;
  //TListTimerClockWidgets = specialize TFPGList<TfraTimer>;
  TIdList = specialize TFPGList<longword>;
  { TClocks }
  TClocks = class(TObject)
  private
    FClockList: TClockList;
    FScrollBox: TScrollBox;
    //FClockWidgets: TClockWidgetList;
    FOrder: TIdList;
    //FActiveTimers: TListTimerClockWidgets;
    FCounterClockID: TSequence;
    //FClocksWidget: TClocksWidget;
    function GetAnySelected: boolean;
    //procedure SetWidget(AValue: TClocksWidget);
    function GetCanselectedMoveDown: boolean;
    function GetCanSelectedMoveUp: boolean;
    procedure SetScrollBox(AValue: TScrollBox);
    procedure Reorder;
  public
    function AddTimer(): TfraTimer;
    constructor Create;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject);
    procedure SaveClocks(Conf: TJsonConfig);
    procedure DeleteSelected;
    //property Widget: TClocksWidget read FClocksWidget write SetWidget;
    property AnySelected: boolean read GetAnySelected;
    procedure RemoveTimer(IdNew: longword);
    procedure MoveSelectedClocksUp;
    procedure MoveSelectedClocksDown;
    procedure GetOrder(AValue: TIdList);
    procedure SetOrder(AValue: TIdList);
    function GetClock(Id: longword): TFraTimer;
    property ScrollBox: TScrollBox read FScrollBox write SetScrollBox;
    property CanSelectedMoveUp: boolean read GetCanSelectedMoveUp;
    property CanSelectedMovDown: boolean read GetCanselectedMoveDown;

  end;

implementation


{ TTimerClock }
{*

procedure TTimerClock.SetWidget(AValue: TfraTimer);
begin
  FWidget := AValue;
end;


function TTimerClock.GetSelected: boolean;
begin
  Result := FWidget.Selected;
end;

procedure TTimerClock.SetId(AValue: integer);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TTimerClock.SetNotifier(AValue: boolean);
begin
  if FNotifier = AValue then
    Exit;
  FNotifier := AValue;
  FWidget.NotifyEnabled := AValue;
end;

constructor TTimerClock.Create;
begin
  FRunning := False;
  FPaused := False;
  FNotifier := False;

  FObservers := TListTimerObservers.Create;

  FShortTimer := TTimer.Create(nil);
  FShortTimer.Interval := 20;
  FShortTimer.Enabled := False;
  FShortTimer.OnTimer := @OnShortTimer;

end;

destructor TTimerClock.Destroy;
begin
  FShortTimer.Free;
  FObservers.Free;
  inherited Destroy;
end;

procedure TTimerClock.OnShortTimer(Sender: TObject);
var
  Elapsed: longword;
  ElapsedMilliseconds: longword;
  Hours: integer;
  Minutes: integer;
  Seconds: integer;
  //Observer: ITimerObserver;
  CounterText: string;
  Progress: single;
begin
  { If the countdown timer is not running, then default to 00:00:00}
  if FRunning = False then
    FWidget.Counter := DEF_COUNTDOWN_CAPTION
  //FEndTickCount
  else if FPaused = False then
  begin
    ElapsedMilliseconds := FEndTickCount - GetTickCount64;
    Elapsed := ElapsedMilliseconds div 1000;
    if Elapsed <= 0 then
    begin
      {Stop(Self);
      //TODO: 0 is not okay
      //TODO: Title is hardcoded
      for Observer in FObservers do
        Observer.Finished(0, 'Countdown timer!', FWidget.Duration);}
      Finish;
      Exit;
    end;
    Seconds := Elapsed mod 60;
    Minutes := Elapsed div 60;
    Hours := Minutes div 60;
    Minutes := Minutes mod 60;
    CounterText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
      ':' + Format('%.2d', [Seconds]);

    if FWidget.Counter <> CounterText then
      FWidget.Counter := CounterText;

    // Calculate percentage progress
    if FNotifier then
    begin
      Progress := 1 - (ElapsedMilliseconds / FOrigTickDuration);
      // Elapsed time can exceed total pending tick duration, in certain cases.
      // The system could go on sleep mode while a timer is running
      // (and while the timer event is being processed) and on
      // waking up, the pre-caculated tick duration could have already been
      // overshot. If that is the case, mark progress as zero, and the next
      // timer event will mark it as completed.
      if Progress < 0 then
        Progress := 0;
      Assert(Progress <= 1);
      PUblishProgress(1 - (ElapsedMilliseconds / FOrigTickDuration));
    end;

  end;
end;


procedure TTimerClock.Start(Sender: TObject);
var
  Duration: TDateTime;
  Hours: word;
  Minutes: word;
  Seconds: word;
  //Milliseconds: word;
begin
  Duration := FWidget.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  if (Hours = 0) and (Minutes = 0) and (Seconds = 0) then
    Exit;

  FShortTimer.Enabled := True;
  if FPaused = False then
  begin
    FStartTickCount := GetTickCount64;

    FEndTickCount :=
      FStartTickCount + (((3600 * longword(Hours)) + (60 * longword(Minutes)) +
      longword(Seconds)) * 1000);

    FOrigTickDuration := FEndTickCount - FStartTickCount;
    if GlobalUserConfig.AutoProgress = True then
    begin
      Notifier:=True;
      NotifyChange(Self);
    end;
  end
  else
  begin
    FStartTickCount := GetTickCount64;
    FEndTickCount := FStartTickCount + FPendingTickCount;
  end;

  FRunning := True;
  FPaused := False;

  with FWidget do
  begin
    PlayButtonEnabled := False;
    PauseButtonEnabled := True;
    StopButtonEnabled := True;
    DurationEnabled := False;
    ImageGreyed := False;

  end;

end;

procedure TTimerClock.Pause(Sender: TObject);
begin
  //ShowMessage('Pause');
  FShortTimer.Enabled := False;

  FPendingTickCount := FEndTickCount - GetTickCount64;
  if FPendingTickCount <= 0 then
    Finish;

  FPaused := True;
  with FWidget do
  begin
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := True;
    DurationEnabled := False;
  end;

end;

procedure TTimerClock.Stop(Sender: TObject);
begin

  FRunning := False;
  FPaused := False;
  FShortTimer.Enabled := False;



  with FWidget do
  begin
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := False;
    DurationEnabled := True;
    ImageGreyed := True;
    Counter := DEF_COUNTDOWN_CAPTION;
  end;

  if FNotifier then
    PublishProgress(TIMER_PROGRESS_FINISHED);

  FEndTickCount := 0;
  FOrigTickDuration := 0;
  FStartTickCount := 0;
end;

procedure TTimerClock.NotifyChange(Sender: TObject);
begin
  if OnNotifyChange <> nil then
  begin
    OnNotifyChange(Self);
    //ShowMessage('Change');
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNotifyChange was found to be Nil');
end;

procedure TTimerClock.Finish;
var
  Observer: ITimerObserver;
begin

  for Observer in FObservers do
  begin
    Observer.TimerFinished(FId);
  end;

  if Notifier then
    PublishProgress(TIMER_PROGRESS_FINISHED);
  Stop(Self);
end;

procedure TTimerClock.PublishProgress(Percent: single);

var
  Observer: ITimerObserver;
begin
  for Observer in FObservers do
    Observer.ProgressUpdate(Percent);

end;

procedure TTimerClock.AddSubscription(aObserver: ITimerObserver);
begin
  FObservers.Add(aObserver);
end;

procedure TTimerClock.RemoveSubscription(aObserver: ITimerObserver);
begin
  FObservers.Remove(aObserver);
end;
*}

{ TClocks }

{*procedure TClocks.SetWidget(AValue: TClocksWidget);
begin
  if AValue = nil then
    Exit;
  if FClocksWidget = AValue then
    Exit;
  FClocksWidget := AValue;

end;*}

function TClocks.GetCanselectedMoveDown: boolean;
var
  Id: longword;
  //Count: integer;
  EncounteredSelected: boolean;
begin
  EncounteredSelected := False;
  //EncounteredUnselected := False;
  for Id in FOrder do
  begin
    if not FClockList.KeyData[Id].Selected then
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

function TClocks.GetCanSelectedMoveUp: boolean;
var
  Id: longword;
  //Count: integer;
  EncounteredUnselected: boolean;
begin
  //EncounteredSelected := False;
  EncounteredUnselected := False;
  for Id in FOrder do
  begin
    if FClockList.KeyData[Id].Selected then
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

procedure TClocks.SetScrollBox(AValue: TScrollBox);
begin
  if AValue = nil then
    Exit;
  if FScrollBox = AValue then
    Exit;
  FScrollBox := AValue;
  //OnTimerStart:=Nil;
end;

procedure TClocks.Reorder;
var
  Id: longword;
  //Index: integer;
  TimerWidget: TfraTimer;
  Filled: integer;
  CountTabOrder: integer;
begin
  //Exit;
  Filled := 0;
  //FScrollBox.AutoScroll:=False;
  //FScrollBox.Visible:=False;
  FScrollBox.Height := FOrder.Count * CLOCK_HEIGHT;
  CountTabOrder:=0;
  for Id in FOrder do
  begin
    //Index := FClockWidgets.IndexOf(Id);
    TimerWidget := FClockList.KeyData[Id];
    //FScrollBox.Height:=Filled + Clock.Height;
    //FScrollBox.VertScrollBar.Range:=Filled + Clock.Height;
    TimerWidget.Top := Filled;
    TimerWidget.TabOrder:=CountTabOrder;
    // + FScrollBox.VertScrollBar.Size - FScrollBox.VertScrollBar.Position;
    if FScrollBox.VertScrollBar.IsScrollBarVisible then
      TimerWidget.Width := FSCrollBox.Width - GetSystemMetrics(SM_CYVSCROLL)
    else
      TimerWidget.Width := FSCrollBox.Width;
    //Clock.He;
    Inc(Filled, TimerWidget.Height);
    Inc(CountTabOrder);
    //if Clock.Kind = 'Timer' then
    //begin
    //TimerWidget := TTimerClockWidget(Clock);

    //TimerWidget.FFrame.lblCountdown.Hint := IntToStr(TimerWidget.FFrame.Top);
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


function TClocks.GetAnySelected: boolean;
var
  Count: integer;
  Clock: TfraTimer;
  //TimerClock: TTimerClock;
begin
  for Count := 0 to FClockList.Count - 1 do
  begin
    Clock := FClockList.Data[Count];
    if Clock = nil then
      ShowMessage('Clock is Nil');
    if Clock.Selected then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TClocks.AddTimer(): TfraTimer;
var
  //NewTimer: TTimerClock;
  Id: longword;
  NewWidget: TfraTimer;
begin
  //NewTimer := TTimerClock.Create;
  Id := FCounterClockID.NextVal;
  //NewTimer.Id := Id;

  //NewWidget := FClocksWidget.AddTimer(Id);
  NewWidget := TfraTimer.Create(FScrollBox);
  NewWidget.Id:=Id;
  FClockList.Add(Id, NewWidget);
  FOrder.Insert(0, Id);
  Reorder;

  //NewTimer.Widget := NewWidget;
  NewWidget.OnNotifyChange := @NotifyChange;
  //NewTimer.AddSubscription(NewWidget.);

  //FClockList.Add(Id, NewWidget);
  //NewWidget.Id:=Id;
  {TODO: This section can be cleaned up}
  NewWidget.OnPlay := @NewWidget.Start;
  NewWidget.OnStop := @NewWidget.Stop;
  NewWidget.OnPause := @NewWidget.Pause;
  NewWidget.OnNotify := @NewWidget.NotifyChange;
  //NewWidget.OnSelect:=@ClockSelected;

  Result := NewWidget;
end;


constructor TClocks.Create;
begin

  //FClockList := TClockWidgetList.Create;
  FOrder := TIdList.Create;

  FClockList := TClockList.Create;
  //FActiveTimers := TListTimerClockWidgets.Create;

  FCounterClockID := TSequence.Create;
  //FClocksWidget := nil;

end;

destructor TClocks.Destroy;
var
  I: integer;
begin
  // Remove any unremoved clocks in the list
  for I := 0 to FClockList.Count - 1 do
  begin
    FClockList.Data[i].Free;
  end;

  FCounterClockID.Free;

  //FActiveTimers.Free;
  FClockList.Free;

  FOrder.Free;
  {for I := 0 to FClockWidgets.Count - 1 do
  begin
    //TODO: Is there a memory leak here?
    FClockWidgets.Data[i].Free;
  end;}

  //FClockWidgets.Free;

  inherited Destroy;
end;

procedure TClocks.NotifyChange(Sender: TObject);
var
  TimerClock: TfraTimer;
  Notifier: TfraTimer;
  Count: integer;
begin

  Notifier := TfraTimer(Sender);
  if not Notifier.NotifyEnabled then
  begin
    Notifier.PublishProgress(TIMER_PROGRESS_FINISHED);
    Notifier.Notifier := False;
    Exit;
  end;


  for Count := 0 to FClockList.Count - 1 do
  begin
    TimerClock := FClockList.Data[Count];
    if TimerClock = nil then
      ShowMessage('Clock is Nil');
    if TimerClock <> Notifier then
    begin
      TimerClock.NotifyEnabled := False;
      if TimerClock.Notifier = True then
      begin
        TimerClock.Notifier := False;
        TimerClock.PublishProgress(TIMER_PROGRESS_FINISHED);
      end;
    end;

  end;

  Notifier.Notifier := True;
  //TODO: Add PublishProgress here
  //Notifier.OnShortTimer(Self);

end;

procedure TClocks.SaveClocks(Conf: TJsonConfig);
var
  TimerClock: TfraTimer;
  Count: integer;
  OrderStrings: TStringList;
  Order: TIdList;
  Id: longword;
  Index: integer;
begin
  Order := TIdList.Create;
  OrderStrings := TStringList.Create;

  GetOrder(Order);

  Conf.SetValue(TIMER_CONF_COUNT, FClockList.Count);
  for Count := 0 to FClockList.Count - 1 do
  begin
    TimerClock := FClockList.Data[Count];
    if TimerClock = nil then
      ShowMessage('Clock is Nil');

    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_TITLE, TimerClock.Caption);
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_HOURS, HourOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_MINUTES, MinuteOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_SECONDS, SecondOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_NOTIFIER, TimerClock.Notifier);

      { While saving, existing IDs of clocks are ignored. They are saved in the
      order they are in the list. The position in the list becomes the new ID.
      The saved order has actual IDs (TimerClock.Id). These have to be
      translated to the new IDs (Count + 1)}

    Index := Order.IndexOf(TimerClock.Id);
    Assert(Index >= 0);
    Order.Items[Index] := Count + 1;

  end;




  for Id in Order do
    OrderStrings.Add(IntToStr(Id));

  Conf.SetValue(TIMER_CONF_ORDER, OrderStrings);

  OrderStrings.Free;
  Order.Free;
end;

procedure TClocks.DeleteSelected;
var
  Id: longword;
  TimerClock: TfraTimer;
  Count: integer;
  //Index: integer;
  IdList: TIdList;
begin
  IdList := TIdList.Create;

  { FPGMap does not have an iterator. So removal of elements have to be done
  with kid-gloves. A seperate list is created to hold IDs of elements to be
  removed from the list. After iteration, removal is done in a separte loop}

  for Count := 0 to FClockList.Count - 1 do
  begin
    TimerClock := FClockList.Data[Count];
    if TimerClock = nil then
      ShowMessage('Clock is Nil');

    if TimerClock.Running then
      TimerClock.Stop(Self);

    if TimerClock.Selected then
    begin
      //RemoveTimer(TimerClock.Id);
      IdList.Add(TimerClock.Id);
      //TimerClock.Free;
    end;

  end;

  for Id in IdList do
  begin
    RemoveTimer(Id);
    //FClockList.Remove(Id);
    //TimerClock.Free;
  end;
  IdList.Free;

end;

procedure TClocks.RemoveTimer(IdNew: longword);
var
  RemovedTimer: TfraTimer;
  Index: integer;
begin
  Index := FClockList.IndexOf(IdNew);
  RemovedTimer := TfraTimer(FClockList.Data[Index]);
  FClockList.Remove(IdNew);
  FOrder.Remove(IdNew);
  RemovedTimer.Free;
  Reorder;
end;

procedure TClocks.MoveSelectedClocksUp;
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

    if FClockList.KeyData[Id].Selected then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      if not (FClockList.KeyData[FOrder.Items[Count - 1]].Selected and
        FClockList.KeyData[FOrder.Items[Count]].Selected) then
        FOrder.Exchange(Count - 1, Count);
    end;
    Inc(Count);
  end;
  Reorder;
end;

procedure TClocks.MoveSelectedClocksDown;
var
  Id: longword;
  Count: integer;
  First: boolean;
begin
  First := True;
  for Count := (FClockList.Count - 1) downto 0 do
  begin
    if First then
    begin
      First := False;
      Continue;
    end;
    Id := FOrder.Items[Count];
    if FClockList.KeyData[Id].Selected then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      if not (FClockList.KeyData[FOrder.Items[Count + 1]].Selected and
        FClockList.KeyData[FOrder.Items[Count]].Selected) then
        FOrder.Exchange(Count + 1, Count);
    end;

  end;
  Reorder;
end;

procedure TClocks.GetOrder(AValue: TIdList);
var
  Id: integer;
begin
  AValue.Clear;
  for Id in FOrder do
  begin
    AValue.Add(Id);
  end;
end;

procedure TClocks.SetOrder(AValue: TIdList);
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

function TClocks.GetClock(Id: longword): TfraTimer;
begin
  Result := FClockList.KeyData[Id];
end;




end.
