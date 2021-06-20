unit log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EventLog, LCLIntf, LCLType, fgl;
//const
  {LOG_NONE = 0;
  LOG_ERROR = 1;
  LOG_WARN = 2;
  LOG_INFO = 3;
  LOG_ALL = 4;}
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
    {%H-}constructor Create;
    {%H-}destructor {%H-}Destroy; override;
  public
    Level: TLogLevel;
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
begin
  InitializeCriticalSection(FLogLock);
  FLogger := TEventLog.Create(nil);
  //Logger := frmMain.evlMain;
  FLogger.LogType := ltFile;
  FLogger.FileName := 'catbell.log';
  Level := LOG_ALL;
  FSubscribers := TLogListenerList.Create;
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
  Logger.Free;
end.
