{---------------------------------------------------------------------------------
This file is part of OpenARCK Project.

   Copyright (C) 2021  Everton Teles
   email: info@evertonteles.com
   home: www.evertonteles.com

OpenArch is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
----------------------------------------------------------------------------------

// History:
  2021 08 20 - ET - Tests of shader .
}  

uniform vec3 BrickColor, MortarColor;
uniform vec2 BrickSize;
uniform vec2 BrickPct;
varying vec2 MCposition;
varying float LightIntensity;
uniform vec2 u_resolution;
void main()
{
    vec2 st = gl_FragCoord.xy/u_resolution.xy;
    vec3 color = vec3(0.0);

    // bottom-left
    vec2 bl = step(vec2(0.1),st);
    float pct = bl.x * bl.y;

    // top-right
     //vec2 tr = step(vec2(0.1),1.0-st);
     //pct *= tr.x * tr.y;

    color = vec3(pct);


    gl_FragColor = vec4(color,1.0);
}

///--------


// ShaderToy Inputs
//uniform vec3      iResolution;           // viewport resolution (in pixels)
//uniform float     iGlobalTime;           // shader playback time (in seconds)

//const float PI=3.14159265358979323846;

//float round( float x ) {
//	float val = mod( x, x );
//	if( val >= 0.5 ){
//		return ceil( x );
//	}else{
//		return floor( x );
//	}
//}


//void main(void)
//{
//	vec2 uv = gl_FragCoord.xy / iResolution.xy;
//		
	// draw vertical lines
	//float stripeVal = floor( mod(gl_FragCoord.x, 7.0) ) == 0.0  ? 1.0 : 0.0;
	//vec4 col = vec4( stripeVal ) * 0.5;
	
	// draw horizontal lines
//	float stripeVal = floor( mod( gl_FragCoord.y, 2.5 ) ) == 0.0  ? 1.0 : 0.0;
//	vec4 col = vec4( stripeVal ) * 0.5;
	
	// draw large round checkerboard
	//float stripeVal = cos( gl_FragCoord.x * 0.1 ) + sin( gl_FragCoord.y * 0.1 );
	//vec4 col = vec4( stripeVal ) * 0.5;
	
	
  	// draw diagonal lines
  
	// rotating diagonal
	//float t = iGlobalTime * 0.5;
	
	// perfectly diagonal
	//float t = PI / -4.0;	

	//float w = 5.0;			  // width (larger value = smaller width)
	//float stripeVal = cos( ( gl_FragCoord.x * cos( t ) * w ) + ( gl_FragCoord.y * sin( t ) * w ) ); 
	//stripeVal = clamp( round( stripeVal ), -1.0, 0.0 );   // clamping gets rid of the random white lines	
	
	//vec4 col = vec4( stripeVal ) * 0.5;	// contrast
//	col += vec4( 0.04, 0.6, 1.0, 0.5 );	  // color
//	gl_FragColor = col;
//}
