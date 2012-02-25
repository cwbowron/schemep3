// ConsoleLockGUI.cpp : Defines the entry point for the application.
//
#include "escheme.h"

#include <tbytevector.h>
#include <mpegfile.h>
#include <id3v2tag.h>
#include <id3v2frame.h>
#include <id3v2header.h>
#include <id3v1tag.h>
#include <tmap.h>
#include <tlist.h>
#include <tstring.h>
#include <xiphcomment.h>
#include <oggfile.h>
#include <flacfile.h>

using namespace std;
using namespace TagLib;

Scheme_Object * scm_taglib_flac_frames( int argc, Scheme_Object * argv[] )
{
    GUARANTEE_CHAR_STRING( "scm_taglib_flac_frames", 0 );
	
    char buffer[1024];
	
    scheme_utf8_encode_to_buffer( SCHEME_CHAR_STR_VAL( argv[0]), 
				  SCHEME_CHAR_STRLEN_VAL( argv[0] ), 
				  buffer, 
				  1024 );
	
    FLAC::File f( buffer );
    Ogg::XiphComment * flacTag = f.xiphComment();

    if ( flacTag )
    {
		Scheme_Object * returnValue	= scheme_null;
		Scheme_Object * tmpTagKey 	= NULL;
		Scheme_Object * tmpTagValue = NULL;
		Scheme_Object * tmpPair		= NULL;
		
		MZ_GC_DECL_REG( 4 );
		MZ_GC_VAR_IN_REG( 0, returnValue );
		MZ_GC_VAR_IN_REG( 1, tmpTagKey );
		MZ_GC_VAR_IN_REG( 2, tmpTagValue );
		MZ_GC_VAR_IN_REG( 3, tmpPair );

		Ogg::FieldListMap::ConstIterator it = flacTag->fieldListMap().begin();
		for(; it != flacTag->fieldListMap().end(); ++it)
		{
		    if(!(*it).second.isEmpty())
		    {
				tmpTagKey 	= scheme_make_utf8_string( (*it).first.toCString() );
				tmpTagValue = scheme_make_utf8_string( (*it).second.toString().toCString() );
				tmpPair		= scheme_make_pair( tmpTagKey, tmpTagValue );
				returnValue = scheme_make_pair( tmpPair, returnValue );
		    }
		}

		MZ_GC_UNREG();			
		return returnValue;
    }

    return scheme_false;
}

Scheme_Object * scm_taglib_id3v2_frames( int argc, Scheme_Object * argv[] )
{	
    GUARANTEE_CHAR_STRING( "taglib-id3v2-frames", 0 );
	
    char buffer[1024];
	
    scheme_utf8_encode_to_buffer( SCHEME_CHAR_STR_VAL( argv[0]), 
				  SCHEME_CHAR_STRLEN_VAL( argv[0] ), 
				  buffer, 
				  1024 );
	
    MPEG::File f( buffer );

    ID3v2::Tag *id3v2tag = f.ID3v2Tag();

    if(id3v2tag) 
    {
		Scheme_Object * returnValue	= scheme_null;
		Scheme_Object * tmpTagKey 	= NULL;
		Scheme_Object * tmpTagValue = NULL;
		Scheme_Object * tmpPair		= NULL;
		
		MZ_GC_DECL_REG( 4 );
		MZ_GC_VAR_IN_REG( 0, returnValue );
		MZ_GC_VAR_IN_REG( 1, tmpTagKey );
		MZ_GC_VAR_IN_REG( 2, tmpTagValue );
		MZ_GC_VAR_IN_REG( 3, tmpPair );

		ID3v2::FrameList::ConstIterator it = id3v2tag->frameList().begin();
		for(; it != id3v2tag->frameList().end(); it++)
		{
			tmpTagKey 	= scheme_make_sized_byte_string( (*it)->frameID().data(), (*it)->frameID().size(), 1 ),
			tmpTagValue = scheme_make_utf8_string( (*it)->toString().toCString( 1 ) ),
			tmpPair		= scheme_make_pair( tmpTagKey, tmpTagValue );							
			returnValue = scheme_make_pair( tmpPair, returnValue );
		}

		MZ_GC_UNREG();		
		return returnValue;
    }
    else
    {
		return scheme_false;
    }
}

Scheme_Object* scheme_initialize( Scheme_Env * env )
{
    Scheme_Env *menv;
	
    menv = scheme_primitive_module( scheme_intern_symbol("taglib_more"),
				    env);
	
    scheme_add_global( "taglib-id3v2-frames",  
		       scheme_make_prim_w_arity( scm_taglib_id3v2_frames, "taglib-id3v2-frames", 1, 1 ),
		       menv );

    scheme_add_global( "taglib-flac-frames",  
		       scheme_make_prim_w_arity( scm_taglib_flac_frames, "taglib-flac-frames", 1, 1 ),
		       menv );
    
    scheme_finish_primitive_module(menv);

    return scheme_void;
}

Scheme_Object* scheme_reload( Scheme_Env * env )
{
    return scheme_initialize( env );
}

Scheme_Object* scheme_module_name()
{
    return scheme_intern_symbol( "taglib_more" );
}
