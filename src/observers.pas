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
unit observers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  {$INTERFACES CORBA}
  ITimerObserver = interface
    procedure TimerFinished(Id: Integer);
    procedure ProgressUpdate(Progress: Single);
    //procedure TimerStarted
  end;

  ITimerSubject = interface
    procedure AddSubscription(aObserver: ITimerObserver);
    procedure RemoveSubscription(aObserver: ITimerObserver);
    //procedure NotifyObservers;
  end;

  TListTimerObservers = specialize TFPGList<ITimerObserver>;

  {IAlarmObserver = interface
    procedure AlarmFinished(Id: Integer);
    procedure ProgressUpdate(Progress: Single);
    //procedure AlarmStarted
  end;

  IAlarmSubject = interface
    procedure AddSubscription(aObserver: IAlarmObserver);
    procedure RemoveSubscription(aObserver: IAlarmObserver);
    //procedure NotifyObservers;
  end;

  TListAlarmObservers = specialize TFPGList<IAlarmObserver>;}

  {ITimerObserver = interface
    procedure Start;
    procedure Pause;
    procedure Stop;
  end;

  ITimerSubject = interface
    procedure AddSubscription(aObserver: ITimerObserver);
    procedure RemoveSubscription(aObserver: ITimerObserver);
    procedure SetCounter(AValue: String);
  end;

  TTimerObserverList = specialize TFPGList<ITimerObserver>;}

implementation

end.

