// ConsoleLockGUI.cpp : Defines the entry point for the application.
//

#define WINVER 0x0501		// Change this to the appropriate value to target other versions of Windows.
#define _WIN32_WINNT 0x0501	// Change this to the appropriate value to target other versions of Windows.

#include <windows.h>
#include <winuser.h>

#include "resource.h"
#include "escheme.h"

#include <WtsApi32.h>

#define SEMA_NAME "system-events-semaphore"

HINSTANCE	g_hinstance 		= NULL;
HWND 		g_hwnd				= 0;
HANDLE		g_native_semaphore 	= NULL;
Scheme_Type g_hotkey_event_type = NULL;
int 		g_hotkey_available 	= 0;

// Scheme_Small_Object g_hotkey_event	= { 0 };

enum eventType { 
	EVENT_TYPE_NONE, 
	EVENT_TYPE_HOTKEY_NEXT, 
	EVENT_TYPE_HOTKEY_PAUSE,
	EVENT_TYPE_CONSOLE_LOCK, 
	EVENT_TYPE_CONSOLE_UNLOCK,
	EVENT_TYPE_LAST
} g_last_event_type = EVENT_TYPE_NONE;

const char * g_event_types[] = { "", "HOTKEY-NEXT", "HOTKEY-PAUSE", "CONSOLE-LOCK", "CONSOLE-UNLOCK" };

void PostEvent( eventType event )
{
	g_hotkey_available 	= 1;
	g_last_event_type	= event;
	ReleaseSemaphore( g_native_semaphore, 1, NULL );			
} 

INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
	case WM_HOTKEY:
		switch ( HIWORD( lParam ) )
		{
		case VK_RIGHT:
			PostEvent( EVENT_TYPE_HOTKEY_NEXT );
			break;

		case VK_SPACE:
			PostEvent( EVENT_TYPE_HOTKEY_PAUSE );
			break;
		}
		break;
	
   case WM_WTSSESSION_CHANGE:
      switch ( wParam )
      {
      case WTS_SESSION_LOCK:	  
			PostEvent( EVENT_TYPE_CONSOLE_LOCK );
			break;
		 
      case WTS_SESSION_UNLOCK:
			PostEvent( EVENT_TYPE_CONSOLE_UNLOCK );
			break;
      }
      break;

   case WM_WINDOWPOSCHANGING:
      {
         WINDOWPOS * pPos = (WINDOWPOS*) lParam;
         pPos->flags |= SWP_HIDEWINDOW;
         pPos->flags &= ~SWP_SHOWWINDOW;
      }
      break;
   
	case WM_INITDIALOG:
		g_hwnd = hDlg;
		WTSRegisterSessionNotification( hDlg, NOTIFY_FOR_ALL_SESSIONS );
		RegisterHotKey( g_hwnd, 1, MOD_ALT | MOD_CONTROL, VK_SPACE );
		RegisterHotKey( g_hwnd, 2, MOD_ALT | MOD_CONTROL, VK_RIGHT );
		return (INT_PTR)TRUE;

	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
		{
			EndDialog(hDlg, LOWORD(wParam));
			return (INT_PTR)TRUE;
		}
		break;
	}
	return (INT_PTR)FALSE;
}

BOOL WINAPI DllMain(
  __in  HINSTANCE hinstDLL,
  __in  DWORD fdwReason,
  __in  LPVOID lpvReserved
)
{
	switch ( fdwReason )
	{
		case DLL_PROCESS_ATTACH:
			g_hinstance = hinstDLL;
			break;

		case DLL_PROCESS_DETACH:
			if ( g_native_semaphore )
			{
				CloseHandle( g_native_semaphore );
				g_native_semaphore = NULL;
			}
			
			if ( g_hwnd )
			{
				EndDialog( g_hwnd, IDOK );
				g_hwnd = 0;
			}		
			break;    
	}
    return TRUE;
}

DWORD WINAPI ThreadProc(  __in  LPVOID lpParameter )
{
    DialogBox( g_hinstance, MAKEINTRESOURCE(IDD_ABOUTBOX), NULL, About );
    return 0;
}

// -----------------------------------------------

Scheme_Object* scm_last_system_event( int argc, Scheme_Object * argv[] )
{
	int n = (int) g_last_event_type;
	g_hotkey_available = 0;
	
	
	if ( ( n > EVENT_TYPE_NONE ) && ( n < EVENT_TYPE_LAST ) )
	{
		return scheme_intern_symbol( g_event_types[n] );
	}
	
	return scheme_false;
}

static int scheme_hotkey_inactive( void * data )
{
	return g_hotkey_available;
}

static void scheme_hotkey_needs_wakeup( void * data, void * fds )
{
	scheme_add_fd_handle( g_native_semaphore, fds, 0 );				
}

Scheme_Object* scheme_initialize( Scheme_Env * env )
{
	DWORD threadID;
	Scheme_Env * menv;
	Scheme_Small_Object * hotkey_event;
	
	g_native_semaphore = CreateSemaphore( NULL, 0, 500, SEMA_NAME );
	
	CreateThread( NULL, 0, ThreadProc, NULL, 0, &threadID );
	
	menv = scheme_primitive_module( scheme_module_name(), env );

	g_hotkey_event_type 		= scheme_make_type("<system-event>");	
	hotkey_event 				= (Scheme_Small_Object*) scheme_malloc( sizeof(Scheme_Small_Object) );
	hotkey_event->iso.so.type	= g_hotkey_event_type;
	hotkey_event->u.int_val 	= (long)g_native_semaphore;	

	scheme_register_extension_global( hotkey_event, sizeof(Scheme_Small_Object) );
	
	scheme_add_evt( g_hotkey_event_type,
			(Scheme_Ready_Fun) scheme_hotkey_inactive,
			(Scheme_Needs_Wakeup_Fun) scheme_hotkey_needs_wakeup,
			NULL, 0);
	
	scheme_add_global( "system-events-event", (Scheme_Object*) hotkey_event, menv );
				 
	scheme_add_global( "last-system-event",  
	                  scheme_make_prim_w_arity( scm_last_system_event, "last-system-event", 0, 0 ),
	                  menv );
	
	scheme_finish_primitive_module( menv );

	return scheme_void;
}

Scheme_Object* scheme_reload( Scheme_Env * env )
{
    return scheme_initialize( env );
}

Scheme_Object* scheme_module_name()
{
	return scheme_intern_symbol( "system-events" );
}
