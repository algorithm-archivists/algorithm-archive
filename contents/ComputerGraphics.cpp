#include<GL/gl.h>
#include<GL/glu.h>
#include<GL/glut.h>
#include<iostream>
#include<math.h>
using namespace std;
float x,x2,y,y2,w,dx,dy,xi,yi,xii,yii;
char d;
bool c=true;
void reshape(int,int);
void DDA(float,float,float,float);
void setPixel(int x, int y)
{
    glColor3f(0.196078f, 0.6f, 0.8f);
    glPointSize(1.0);
    glBegin(GL_POINTS);
    glVertex2f(x,y);
    glEnd();
    glFlush();
    //glFlush();
}
void mouse(int button, int state, int mx, int my)
{
    if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN && c)
    {
        x = mx-360;
        y = 360-my;
        setPixel(x,y);
        c=false;
    }
    else if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN && !c)
    {
        x2 = mx-360;
        y2 = 360-my;
        setPixel(x2,y2);
        c=true;
    }
    glutPostRedisplay();
 }

void display()
{
    //glClearColor(1,1,1,0);
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
    glBegin(GL_LINES);
    glColor3f(0.196078f, 0.6f, 0.8f);
    glVertex2f(-360,0);
    glVertex2f(360,0);
    glVertex2f(0,360);
    glVertex2f(0,-360);
    glEnd();

if(!c)
{
    glBegin(GL_POINTS);
    glColor3f(0.196078f, 0.6f, 0.8f);
    DDA(x,y,x2,y2);
    float slope=(y2-y)/(x2-x),slope2;
    slope2=-1/slope;
    if(slope>1)
    {
        {
            xi=x-1;
            yi=y-slope2;
        }
        dx=xi-x;
        xii=dx;
        dy=yi-y;
        yii=dy;
        while(w>=sqrt(dx*dx+dy*dy))
        {
            DDA(x-dx,y-dy,x2-dx,y2-dy);
            dx=dx+xii;
            dy=dy+yii;
        }
    }
    else
    {
        {
            yi=y+1;
            xi=1/slope2+x;
        }
        dx=x-xi;
        xii=dx;
        dy=y-yi;
        yii=dy;
        while((w>=sqrt(dx*dx+dy*dy)))
        {
            DDA(x-dx,y-dy,x2-dx,y2-dy);
            dx=dx+xii;
            dy=dy+yii;
        }
    }
    glEnd();
}
    glFlush();
}

int main(int argc,char **argv)
{
    cout<<"Enter the width of the Bar "<<endl;
    cin>>w;
    glutInit(&argc,argv);
    glutInitDisplayMode(GLUT_RGB);
    glutInitWindowSize(720,720);
    glutInitWindowPosition(200,20);
    glutCreateWindow("Parallel Lines Algorithm");
    glutDisplayFunc(display);
    glutMouseFunc(mouse);
    glutReshapeFunc(reshape);
    glutMainLoop();
}
void reshape(int w,int h)
{
    glViewport(0,0,(GLsizei)w,(GLsizei)h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(-360,360,-360,360);
    glMatrixMode(GL_MODELVIEW);
}
void DDA(float x,float y,float x2,float y2)
{
	float dx = x2 - x;
	float dy = y2 - y;
	float x1 = x;
	float y1 = y;
	float step = 0;
	if(abs(dx) > abs(dy))
	{
	  step = abs(dx);
	} else
	{
	  step = abs(dy);
	}
	float xInc = dx/step;
	float yInc = dy/step;
	glBegin(GL_POINTS);
	for(float i = 1; i <= step; i++)
	{
		glVertex2i(x1, y1);
		x1 += xInc;
		y1 += yInc;
	}
	c=0;
	glEnd();
	glFlush();
}
