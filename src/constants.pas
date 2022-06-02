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

unit constants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Graphics, DateTimePicker;

type
  { Used for settings }
  TTaskbarIconType = (TaskbarAppIcon, TaskbarOverlayIcon);

const

  {Audio}

  TableSize = 200;

  MAX_VOLUME = 100;
  MIN_VOLUME = 0;

  DEFAULT_VOLUME = 90;
  VOLUME_LOG_BASE = 10;

  INVALID_SOUNDPOOL_INDEX = -1;

  REFILL_BUFFER_SIZE = 4096;

  {$IF defined(windows) }
  WIN_HINT = 'Hint: Is this DLL missing?';
  {$ELSEIF Defined(unix)}
  LX_HINT = 'Hint: Install packages libportaudio2, libsndfile1 and libmpg123-0';
  {$ENDIF}

  AUDIO_LATENCY_MIN = 0;
  AUDIO_LATENCY_MAX = 50;

  {Main}

  FORM_MIN_WIDTH = 600;
  FORM_MIN_HEIGHT = 300;

  TICON_GREEN_INDEX: integer = 0;
  TRAY_PROGRESS_ICON_COUNT = 24;

  PROGRESS_COLOUR = $4352E2;
  PROGRESS_GLOSS_OPACITY = 60;

  PROGRESS_COMPLETED = 2.0;

  TRAY_OUTLINE_INSET = 2;

  OVERLAY_STROKE_WIDTH = 16;
  OVERLAY_STROKE_COLOUR = $515151;
  OVERLAY_BACKGROUND_COLOUR = $5CEFFF;

  TRAY_CENTRE_INNER_RADIUS = 2;
  TRAY_CENTRE_OUTER_RADIUS = 4;
  TRAY_BASE_WIDTH = 16;

  WIDGET_ICON_WIDTH = 24;

  RAD_MULTIPLIER = 16;

  LARGE_ICON_SIZE = 256;

  LAST_TRAY_ICON_DEFAULT = -1;

  TIMER_PROGRESS_OFFTRAY: single = 3.0;

  PANEL_TIMERCOUNT = 0;
  PANEL_AUDIO = 1;
  PANEL_MESSAGE = 2;

  HELP_URL = 'https://github.com/vipincherian/catbell/blob/master/doc/help.md';

  {Timer frame }

  TIMER_IMG_GREY_TIMER: integer = 0;
  TIMER_IMG_COLOUR_TIMER: integer = 1;
  TIMER_IMG_NOTIFY_YES: integer = 2;
  TIMER_IMG_NOTIFY_NO: integer = 3;

  CLOCK_HEIGHT = 38;
  DEF_COUNTDOWN_CAPTION: string = '00:00:00';
  TIMER_PROGRESS_FINISHED: single = 2.0;

  TIMER_CONF_CLOCKS = 'clocks';
  TIMER_CONF_TIMERS = 'timers';
  TIMER_CONF_TITLE = 'timer_title';
  TIMER_CONF_TIME = 'time';
  TIMER_CONF_DURATION = 'duration';
  TIMER_CONF_NOTIFIER = 'notifier';
  TIMER_CONF_COUNT = 'count';

  TIMER_CONF_SOUND_NONE = INVALID_SOUNDPOOL_INDEX;
  TIMER_CONF_SOUND_DEFALUT = 0;
  TIMER_CONF_SOUND_CUSTOM = 1;

  TIMER_CONF_SOUNDTYPE = 'audio_type';
  TIMER_CONF_SOUNDFILEPATH = 'audio_file';
  TIMER_CONF_SOUNDLENGTH = 'audio_duration';
  TIMER_CONF_SOUNDLOOP = 'audio_loop';


  TIMER_CONF_MODALALERT = 'modal_alert';
  TIMER_CONF_TRAYNOTIFICATION = 'tray_notification';

  TIMER_CONF_PENDINGTICKCOUNT = 'pending_tick_count';
  TIMER_CONF_ENDTIME = 'end_time';
  TIMER_CONF_ORIGTICKCOUNT = 'orig_tick_count';

  TIMER_CONF_RUNNING = 'running';
  TIMER_CONF_PAUSED = 'paused';

  TIMER_CONF_METRONOME = 'metronome';

  { For auto arranging }
  TIMER_PADDING = 5;
  TIMER_REPORT_PADDING = 20;

  { Settings }
  AUDIO_ABORT_SHORT_WAIT = 2000;
  AUDIO_ABORT_LONG_WAIT = 5000;

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

  ADJ_DIFF = 'adjust_diff';
  DEF_ADJ_DIFF = DEF_TIMER_DURATION;
  ADJ_COMPLETEBY = 'adust_completeby';
  DEF_ADJ_COMPLETEBY = DEF_TIMER_DURATION;

  USE_DEFAULT_AUDIO_DEVICE = 'audio_device_default';
  DEF_USE_DEFAULT_AUDIO_DEVICE = True;
  AUDIO_HOSTAPI_NAME = 'audio_hostapi_name';
  DEF_AUDIO_HOSTAPI_NAME = '';
  AUDIO_DEVICE_NAME = 'audio_device_name';
  DEF_AUDIO_DEVICE_NAME = '';

  TASKBAR_ICON_TYPE = 'taskbar_appicon';
  {$IF defined(windows)}
  DEF_TASKBAR_ICON_TYPE = TaskbarOverlayIcon;
  {$ELSE}
  DEF_TASKBAR_ICON_TYPE = TaskbarAppIcon;
  {$ENDIF}

  USE_DEFAULT_SOUND = 'use_default_sound';
  DEF_USE_DEFAULT_SOUND = True;

  LOOP_SOUND = 'loop_sound';
  DEF_LOOP_SOUND = False;

  VOLUME_LEVEL = 'volume';
  DEF_VOLUME_LEVEL = DEFAULT_VOLUME;

  TRAY_ICON_SIZE = 'tray_icon_size';
  APP_ICON_SIZE = 'app_icon_size';

  OVERRIDE_TRAY_ICON_SIZE = 'override_tray_icon_size';
  DEF_OVERRIDE_TRAY_ICON_SIZE = False;
  OVERRIDE_APP_ICON_SIZE = 'override_app_icon_size';
  DEF_OVERRIDE_APP_ICON_SIZE = False;

  RESTART_FROM_FINISH = 'restart_from_finish';
  DEF_RESTART_FROM_FINISH = True;

  OVERRIDE_LATENCY = 'override_latency';
  DEF_OVERRIDE_LATENCY = False;

  AUDIO_LATENCY = 'latency';
  DEF_AUDIO_LATENCY = 0;
  AUDIO_LATENCY_DIVISOR = 1000;

  {Adjust}

  ADJUST_SHORTEN = 0;
  ADJUST_EXTEND = 1;
  ADJUST_STOPBY = 2;
  ADJUST_STOPBY_TEXT = 'Stop timer by';

  {Edit}

  TYPE_DURATION = 0;
  TYPE_BY = 1;

  SOUND_DEFAULT = 0;
  SOUND_CUSTOM = 2;
  SOUND_NONE = 1;

  {Options}

  LSVADUIO_INDEX_HOSTAPI: integer = 0;
implementation

end.

