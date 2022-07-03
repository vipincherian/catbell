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

unit log;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, EventLog, LCLIntf, LCLType, fgl, constants, Dialogs;

type
  TLogLevel = (LOG_NONE, LOG_ERROR, LOG_WARN, LOG_INFO, LOG_ALL);
  TNotifyLogEvent = procedure(Msg: string) of object;
  TLogListenerList = specialize TFPGList<TNotifyLogEvent>;
  { TLogger }

  TLogger = class(TObject)
  private
    FLogger: TEventLog;
    FLogLock: TCriticalSection;
    FSubscribers: TLogListenerList;

  public
    Level: TLogLevel;
    constructor Create;
    destructor Destroy; override;
    procedure Info(const Msg: string);
    procedure Info(const Fmt: string; Args: array of const);
    procedure Debug(const Msg: string);
    procedure Debug(const Fmt: string; Args: array of const);
    procedure Warning(const Msg: string);
    procedure Warning(const Fmt: string; Args: array of const);
    procedure Error(const Msg: string);
    procedure Error(const Fmt: string; Args: array of const);
    procedure Subscribe(const Callback: TNotifyLogEvent);

  end;

var
  Logger: TLogger = nil;

implementation

{ TLogger }


constructor TLogger.Create;
var
  AppConfigDir: string = '';
begin
  { Constructor cannot be hidden unless it is made strict private.
  Constructor will proceed with creation only if the single instance is not
  created yet. Once created, the same instance is returned.}
  if not Assigned(Logger) then
  begin
    inherited Create;
    InitializeCriticalSection(FLogLock);
    FLogger := TEventLog.Create(nil);
    FLogger.LogType := ltFile;

    {If application configuration directory does not exist, create it}

    AppConfigDir := GetAppConfigDir(False);

    if not DirectoryExists(AppConfigDir) then
      if not CreateDir(AppConfigDir) then
        ShowMessage('Failed to create application configuration directory ' +
          AppConfigDir);

    FLogger.FileName := AppConfigDir + 'catbell.log';
    Level := LOG_ALL;
    FSubscribers := TLogListenerList.Create;
  end
  else
    Self := Logger;
end;

destructor TLogger.Destroy;
begin
  FSubscribers.Free;
  FLogger.Free;
  DeleteCriticalSection(FLogLock);
  inherited Destroy;
end;

procedure TLogger.Info(const Msg: string);
var
  Callback: TNotifyLogEvent;
begin
  if Level >= LOG_INFO then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Info(Msg);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
  for Callback in FSubscribers do
  begin
    Callback(Msg);
  end;

end;

procedure TLogger.Info(const Fmt: string; Args: array of const);
begin
  if Level >= LOG_INFO then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Info(Fmt, Args);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
end;

procedure TLogger.Debug(const Msg: string);
begin
  if Level >= LOG_ALL then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Debug(Msg);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
end;

procedure TLogger.Debug(const Fmt: string; Args: array of const);
begin
  if Level >= LOG_ALL then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Debug(Fmt, Args);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
end;

procedure TLogger.Warning(const Msg: string);
var
  Callback: TNotifyLogEvent;
begin
  if Level >= LOG_WARN then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Warning(Msg);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
  for Callback in FSubscribers do
  begin
    Callback(Msg);
  end;
end;

procedure TLogger.Warning(const Fmt: string; Args: array of const);
begin
  if Level >= LOG_WARN then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Warning(Fmt, Args);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
end;

procedure TLogger.Error(const Msg: string);
var
  Callback: TNotifyLogEvent;
begin
  if Level >= LOG_ERROR then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Error(Msg);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
  for Callback in FSubscribers do
  begin
    Callback(Msg);
  end;
end;

procedure TLogger.Error(const Fmt: string; Args: array of const);
begin
  if Level >= LOG_ERROR then
  begin
    EnterCriticalSection(FLogLock);
    try
      FLogger.Error(Fmt, Args);
    finally
      LeaveCriticalSection(FLogLock);
    end;
  end;
end;

procedure TLogger.Subscribe(const Callback: TNotifyLogEvent);
begin
  FSubscribers.Add(Callback);
end;

initialization
  Logger := TLogger.Create;

finalization
  FreeAndNil(Logger);
end.
