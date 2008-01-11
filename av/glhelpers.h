/*
 *  glhelpers.h
 *  musem
 *
 *  Created by Srikumar Subramanian on 12/08/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef __GLHELPERS_H__
#define __GLHELPERS_H__

#ifdef _WIN32
#include <gl/glew.h>
#endif
//#include <GLUT/glut.h>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <math.h>

typedef GLhandleARB ShaderObj;
typedef GLhandleARB ProgramObj;

inline void reportGL() {
	std::cout 
	<< "OpenGL version " << glGetString(GL_VERSION) << "\n"
	<< "OpenGL renderer - " << glGetString(GL_RENDERER) << "\n"
;//	<< "OpenGL extensions - " << glGetString(GL_EXTENSIONS) << "\n\n";
}


inline void checkGL( const char *msg) {
	GLenum err = glGetError();
	if ( err != 0 ) {
		std::cerr << msg << " error:  " << err << " '" << gluErrorString(err) << "'\n";
		MessageBoxA( NULL, (LPCSTR)gluErrorString(err), msg, MB_OK );
		throw -1;
	}
}

inline void checkGL(  const char *msg, GLenum status, GLenum value ) {
	if ( status != value ) {
		std::cerr << msg << " error: Status is " << status << ". It is expected to be " << value << ".\n";
		throw -1;
	}
}

/**
 * Scope is used to measure the time taken by any static
 * scope. You describe the scope using a printf-style
 * argument list to the constructor. The destructor will
 * print the time consumed between the constructor and
 * the destructor.
 */
class Scope {
public:
	Scope(const char *desc, ...);
	~Scope();
private:
	long long beginTime;
	static int g_tablevel;
	static void *g_clock;
	static void tab();
};

#ifdef _WIN32
class WithGLEW {
public:
	WithGLEW( GLEWContext *g ) : m_g(g) {}
	GLEWContext *glewGetContext() const { return m_g; }
	GLEWContext *operator()() const { return m_g; }
protected:
	GLEWContext *m_g;
};
#endif

template <class T>
inline T checkGLproc( T proc, const char *name ) {
	if ( !proc ) {
		std::cerr << "Error: proc " << name << " doesn't exist!\n";
		throw -1;
	}
	
	return proc;
}

#ifdef _WIN32
#define glfn(x) checkGLproc( x, #x )
#else
#define glfn(x) x
#endif

class Texture : public WithGLEW {
public:
	Texture( GLEWContext *g, GLuint _width, GLuint _height, bool _mipmap = false ) : WithGLEW(g), width(_width), height(_height), mipmap(_mipmap) {
		glGenTextures(1, &tex);
		checkGL("Create texture");
		try {
			glBindTexture(GL_TEXTURE_2D, tex);
			glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,  width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
			checkGL("glTexImage2D");
			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
			
			if ( mipmap ) {
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
				// Wisdom: Tried to use GL_GENERATE_MIPMAP here instead of glGenerateMipmapEXT
				// but that didn't work. It looks  like we have to call glGenerateMipmapEXT()
				// explicitly every time the texture gets drawn to.
				glGenerateMipmapEXT(GL_TEXTURE_2D);
				checkGL("glGenerateMipmapEXT");			
			} else {
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);				
			}
		}
		catch ( ... ) {
			glDeleteTextures(1,&tex);
			throw;
		}
		
	}
	
	~Texture() {
		glDeleteTextures(1,&tex);
	}
	
	void bind() const {
		glEnable( GL_TEXTURE_2D );
		glBindTexture( GL_TEXTURE_2D, tex );
		checkGL("Bind texture");		
	}
	
	void update() const {
		if ( mipmap ) {
			bind();
			glGenerateMipmapEXT(GL_TEXTURE_2D);
			checkGL("glGenerateMipmapEXT");
		}
	}
	
	void draw( float alpha ) const {
		bind();
		glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
		glColor4f(1,1,1,alpha);
		glBegin( GL_QUADS );
		{
			glTexCoord2f(0,0);
			glVertex2f(0,0);
			glTexCoord2f(1,0);
			glVertex2f(1,0);
			glTexCoord2f(1,1);
			glVertex2f(1,1);
			glTexCoord2f(0,1);
			glVertex2f(0,1);
		}
		glEnd();
	}
	
	GLuint tex;
	GLuint width, height;
	bool mipmap;
};

class BindTexture {
public:
	BindTexture( const Texture &tex ) {
		tex.bind();
	}
	
	~BindTexture() {
		glBindTexture( GL_TEXTURE_2D, 0 );
	}
};

class FrameBuffer : public WithGLEW {
public:
	GLuint fbo, depthbuffer;
	GLuint width, height;

	FrameBuffer( GLEWContext *g, GLuint _width, GLuint _height ) : WithGLEW(g), width(_width), height(_height) {
		glGenFramebuffersEXT(1, &fbo);
		checkGL("Create FBO");
		try {
			glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, fbo );
			
			// Add a depth buffer to the FBO.
			GLuint depthbuffer;
			glGenRenderbuffersEXT(1, &depthbuffer);
			glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, depthbuffer);
			glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, width, height);
			glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, depthbuffer);
			checkGL("FBO depth attachment");			
			glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
		}
		catch ( ... ) {
			glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
			glDeleteRenderbuffersEXT(1, &depthbuffer);
			glDeleteFramebuffersEXT( 1, &fbo );
			throw;
		}
	}
	
	~FrameBuffer() {
		glDeleteRenderbuffersEXT(1, &depthbuffer);
		glDeleteFramebuffersEXT( 1, &fbo );
	}	
};

class Render {
public:
	Render( GLuint x, GLuint y, GLuint width, GLuint height ) {
		enter(x,y,width,height);
	}
	
	~Render() {
		glFlush();
	}
	
private:
	void enter( GLuint x, GLuint y, GLuint width, GLuint height ) {
		glViewport( x, y, width, height );
		glClearColor(0,0,0,1);
		glClear( GL_COLOR_BUFFER_BIT );
	} 
};

class RenderToTexture : public WithGLEW {
public:
	const FrameBuffer &fbo;
	const Texture &tex;
	GLuint colorix;

	RenderToTexture( const FrameBuffer &_fbo, const Texture &_tex, GLuint _colorix = 0 ) : WithGLEW(fbo.glewGetContext()), fbo(_fbo), tex(_tex), colorix(_colorix) {
		glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo.fbo);
		checkGL("Bind FBO");
		glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT + colorix, GL_TEXTURE_2D, tex.tex, 0);
		checkGL("Set FBO color attachment");
		GLenum status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
		checkGL( "FBO status", status, GL_FRAMEBUFFER_COMPLETE_EXT );
		glDrawBuffer( GL_COLOR_ATTACHMENT0_EXT + colorix );
		checkGL("Set draw buffer");
		glPushAttrib(GL_VIEWPORT_BIT);
		glViewport(0,0,fbo.width, fbo.height);		
	}
	
	~RenderToTexture() {
		checkGL("RenderToTexture");
		glFlush();
		glPopAttrib();
		glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
		tex.update();
	}
};

class Shader : public WithGLEW {
public:
	GLenum type;

	Shader( GLEWContext *g, GLenum _type ) : WithGLEW(g), type(_type), shader(0), compiled(false) {
		shader = glfn(glCreateShaderObjectARB)(type);
		checkGL( type == GL_VERTEX_SHADER_ARB ? "Create vertex shader" : "Create fragment shader" );
	}
	
	~Shader() {
		glfn(glDeleteObjectARB)(shader);
	}
	
	void source( const GLchar *src ) {
		glfn(glShaderSourceARB)( shader, 1, &src, NULL );
		checkGL("Shader source");
	}
	
	void sourceFile( const char *file ) {
		FILE *f = fopen(file,"rt");
		if ( !f ) throw -1;
		fseek(f,0,SEEK_END);
		fpos_t len = ftell(f);
		fseek(f,0,SEEK_SET);
		
		GLchar *mem = (GLchar*)malloc( (size_t)(len+1) );
		fread( mem, (size_t)len, 1, f );
		mem [len] = '\0';
		fclose(f);
		try {
			source( mem );			
			free( mem );
		}
		catch ( ... ) {
			free(mem);
			throw;
		}
	}
	
	ShaderObj operator()() const {
		return compile();
	}
	
protected:
	ShaderObj shader;
	mutable bool compiled;
	
	ShaderObj compile() const {
		if ( !compiled ) {
			glfn(glCompileShaderARB)(shader);
			
			GLint status;
			glfn(glGetObjectParameterivARB)( shader, GL_OBJECT_COMPILE_STATUS_ARB, &status );
			if ( status == GL_FALSE ) {
				std::cerr << "Shader compilation failed! Log follows -\n---------------\n";
				GLsizei maxLength = 0;
				glfn(glGetObjectParameterivARB)(shader, GL_OBJECT_INFO_LOG_LENGTH_ARB, &maxLength);
				GLchar *log = (GLchar*)malloc( maxLength+1);
				GLsizei len = 0;
				glfn(glGetInfoLogARB)( shader, maxLength, &len, log );
				log[len-1] = '\0';
				std::cerr << log << "\n--------------\n";	
				MessageBoxA( NULL, log, "Shader error", MB_OK );
				free(log);
				throw -1;
			}
			compiled = true;
		}
		
		return shader;
	}
	
	friend class Program;
};

class FragmentShader : public Shader {
public:
	FragmentShader( GLEWContext *g ) : Shader( g, GL_FRAGMENT_SHADER_ARB ) {}
};

class VertexShader : public Shader {
public:
	VertexShader( GLEWContext *g ) : Shader( g, GL_VERTEX_SHADER_ARB ) {}
};

class Program : public WithGLEW {
public:
	Program( GLEWContext *g ) : WithGLEW(g), linked(false) {
		program = glfn(glCreateProgramObjectARB)();
		checkGL("Create program");
	}
	
	~Program() {
		glfn(glDeleteObjectARB)(program);
	}
	
	void attach( Shader &sh ) {
		glfn(glAttachObjectARB)( program, sh() );
		checkGL("Attach shader");
	}
	
	void detach( Shader &sh ) {
		glfn(glDetachObjectARB)( program, sh.shader );
		checkGL("Detach shader");
	}
	
	ProgramObj operator()() const {
		return link();
	}

	void engage() {
		glfn(glUseProgramObjectARB)( link() );
	}

	void disengage() {
		glfn(glUseProgramObjectARB)(0);
	}
	
private:
	ProgramObj program;	
	mutable bool linked;
	
	ProgramObj link() const {
		if ( !linked ) {
			glfn(glLinkProgramARB)(program);
			
			GLint status;
			glGetObjectParameterivARB( program, GL_OBJECT_LINK_STATUS_ARB, &status );
			
			if ( status == GL_FALSE ) {
				std::cerr << "Program linking failed! Log follows -\n----------------\n";
				
				GLsizei maxLength = 0;
				glfn(glGetObjectParameterivARB)(program, GL_OBJECT_INFO_LOG_LENGTH_ARB, &maxLength);
				GLchar *log = (GLchar*)malloc( maxLength+1);
				GLsizei len = 0;
				glfn(glGetInfoLogARB)( program, sizeof(log), &len, log );
				log[len] = '\0';
				std::cerr << log << "\n--------------\n";	
				MessageBoxA( NULL, log, "Program link error", MB_OK );
				free(log);
				throw -1;
			}
			linked = true;
		}
		
		return program;
	}
};

class WithProgram {
public:
	Program &program;

	WithProgram( Program &prog ) : program(prog) {
		program.engage();
		checkGL("glUseProgram");
	}
	
	~WithProgram() {
		program.disengage();
	}
};

class Uniform : public WithGLEW {
public:
	GLint name;
	
	Uniform( Program &program, const GLchar *_name ) : WithGLEW(program.glewGetContext()) {
		name = glfn(glGetUniformLocationARB)( program(), _name );
		checkGL( _name );
	}
	
	~Uniform() {
	}
	
	Uniform &operator=( GLfloat v ) {
		glfn(glUniform1fARB)( name, v );		
		return *this;
	}
	
	Uniform &operator=( GLint v ) {
		glfn(glUniform1iARB)( name, v );		
		return *this;
	}
	
	void set( int vecN, const GLfloat *v, int count = 1 ) {
		switch ( vecN ) {
			case 1: glfn(glUniform1fvARB)( name, count, v ); break;
			case 2: glfn(glUniform2fvARB)( name, count, v ); break;
			case 3: glfn(glUniform3fvARB)( name, count, v ); break;
			case 4: glfn(glUniform4fvARB)( name, count, v ); break;
			default : 
				std::cerr << "Error: No vector type > 4 in size! You asked for " << vecN << "\n";
				throw -1;
		}
	}

	void set( int vecN, const GLint *v, int count = 1 ) {
		switch ( vecN ) {
			case 1: glfn(glUniform1ivARB)( name, count, v ); break;
			case 2: glfn(glUniform2ivARB)( name, count, v ); break;
			case 3: glfn(glUniform3ivARB)( name, count, v ); break;
			case 4: glfn(glUniform4ivARB)( name, count, v ); break;
			default : 
				std::cerr << "Error: No vector type > 4 in size! You asked for " << vecN << "\n";
				throw -1;
		}
	}
	
	void setMatrix( int matN, const GLfloat *v, int count = 1 ) {
		switch ( matN ) {
			case 2: glfn(glUniformMatrix2fvARB)( name, count, GL_FALSE, v ); break;
			case 3: glfn(glUniformMatrix3fvARB)( name, count, GL_FALSE, v ); break;
			case 4: glfn(glUniformMatrix4fvARB)( name, count, GL_FALSE, v ); break;
			case -2: glfn(glUniformMatrix2fvARB)( name, count, GL_TRUE, v ); break;
			case -3: glfn(glUniformMatrix3fvARB)( name, count, GL_TRUE, v ); break;
			case -4: glfn(glUniformMatrix4fvARB)( name, count, GL_TRUE, v ); break;
			default:
				std::cerr << "Error: Matrix types are 2x2 or 3x3 or 4x4. You asked for " << matN << "x" << matN << "!\n";
				throw -1;
		}
	}
};

class Attrib : public WithGLEW {
public:
	GLint name;
	
	Attrib( const Program &program, const GLchar *_name ) : WithGLEW(program.glewGetContext()) {
		name = glfn(glGetAttribLocationARB)( program(), _name );
		checkGL("glGetAttribLocation");
	}
	
	~Attrib() {
	}
	
	void operator()( GLfloat v1 ) {
		glfn(glVertexAttrib1fARB)( name, v1 );
	}
	
	void operator()( GLfloat v1, GLfloat v2 ) {
		glfn(glVertexAttrib2fARB)( name, v1, v2 );
	}

	void operator()( GLfloat v1, GLfloat v2, GLfloat v3 ) {
		glfn(glVertexAttrib3fARB)( name, v1, v2, v3 );
	}
	
	void operator()( GLfloat v1, GLfloat v2, GLfloat v3, GLfloat v4 ) {
		glfn(glVertexAttrib4fARB)( name, v1, v2, v3, v4 );
	}
};

class Projection {
public:
	Projection() {
		glMatrixMode( GL_PROJECTION );
		glPushMatrix();
//XX		glLoadIdentity();		
	}
	
	~Projection() {
		glMatrixMode( GL_PROJECTION );
		glPopMatrix();
	}
};

class Model {
public:
	Model() {
		glMatrixMode( GL_MODELVIEW );
		glPushMatrix();
	}
	
	~Model() {
		glMatrixMode( GL_MODELVIEW );
		glPopMatrix();
	}

	void scale( const float aspectRatio ) {
		const float xarf = (aspectRatio > 1.0f) ? 1.0f : aspectRatio;
		const float yarf = (aspectRatio > 1.0f) ? 1.0f/aspectRatio : 1.0f;
		glScalef( xarf, yarf, 1.0f );
	}

};

#if 0
class Image {
public:
	Image( const wchar_t *file ) {
		char nfile[512];
		sprintf( nfile, "%ls", file );
		nfile[511] = '\0';
		tex = ilutGLLoadImage((ILstring)nfile);
		width = ilGetInteger( IL_IMAGE_WIDTH );
		height = ilGetInteger( IL_IMAGE_HEIGHT );
		float scale = max(width,height);//sqrt(float(width*height));
		xscale = width/scale;
		yscale = height/scale;
	}
	
	~Image() {
		glDeleteTextures(1,&tex);
		tex = 0;
	}
	
	void draw( float alpha ) {
		glEnable( GL_TEXTURE_2D );
		glBindTexture(GL_TEXTURE_2D, tex);
		glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
		glMatrixMode( GL_MODELVIEW );
		glPushMatrix();
		glScalef( xscale, yscale, 1.0 );
		glColor4f(1,1,1,alpha);
		glBegin( GL_QUADS );
		{
			glTexCoord2f(0,0);
			glVertex3f(0,0,0);
			glTexCoord2f(1,0);
			glVertex3f(1,0,0);
			glTexCoord2f(1,1);
			glVertex3f(1,1,0);
			glTexCoord2f(0,1);
			glVertex3f(0,1,0);
		}
		glEnd();		
		glPopMatrix();
	}
	
	GLuint tex;
	int width, height;
	float xscale, yscale;
};
#endif

class Blend {
public:
	Blend( GLenum srcfn, GLenum destfn ) {
		glEnable( GL_BLEND );
		glBlendFunc( srcfn, destfn );
	}

	~Blend() {
		glDisable( GL_BLEND );
	}
};

class Depth
{
public:
	Depth( GLenum func, float clearVal ) { 
		glEnable( GL_DEPTH_TEST ); 
		glDepthFunc( func );
		glClearDepth( clearVal );
		glClear( GL_DEPTH_BUFFER_BIT );
	}

	~Depth() { glDisable( GL_DEPTH_TEST ); }
};

class PlayingFPS
{
public: 
	PlayingFPS() {
		numRenders = nextRender = 0;
		playing_fps = 0;
	}

	double tick( double time_secs ) {
		recentRenders[nextRender] = time_secs;
		nextRender = (nextRender+1) % maxRenders;
		numRenders = (numRenders < maxRenders) ? (numRenders + 1) : numRenders;

		// Compute the playing_fps;
		if ( numRenders > 0 ) {
			if ( numRenders >= maxRenders ) {
				// The nextRender location is the oldest.
				playing_fps = maxRenders / (recentRenders[(maxRenders + nextRender-1) % maxRenders] - recentRenders[nextRender]);
			} else {
				playing_fps = nextRender / (recentRenders[nextRender-1] - recentRenders[0]);
			}
		} else {
			playing_fps = 0.0;
		}

		return playing_fps;
	}

	double operator()() const {
		return playing_fps;
	}

private:
	enum { maxRenders = 120 };
	int numRenders, nextRender;
	double recentRenders[maxRenders];
	double playing_fps;
};

#endif // __GLHELPERS_H__

