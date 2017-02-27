
float phase=0.;
float zoom=30;
float dphase=0.01;

void setup()
{
  size(640, 360);
  background(0);
  println("starting plot2d...");
  println("click on the window then press 's' to take a screenshot");
  println("use LEFT/RIGHT arrows to increase/decrease rotation speed");
}

void draw()
{
  background(10,10,50);
  //fill(150);

  translate(width/2, height/2);
  
  // axes
  stroke(255,80);strokeWeight(1);
  line(-width/2, 0, width, 0);
  line(0, -height/2, 0, height/2);
  

  int tickl=2;
   
  // grid x
  for(int ng=-round(width/2.0/zoom); ng<=round(width/2.0/zoom); ++ng)
  {
    stroke(255,30);
    line(ng*zoom, -height/2, ng*zoom, height/2);
    stroke(255,100);
    line(ng*zoom, -tickl, ng*zoom, tickl);
    textAlign(RIGHT, TOP); textSize(10);
    text(ng, -2+ng*zoom, 0);
  }
  // grid y
  for(int ng=-round(height/2.0/zoom); ng<=round(height/2.0/zoom); ++ng)
  {
    stroke(255,30);
    line(-width/2, ng*zoom, width/2, ng*zoom);
    stroke(255,100);    
    line(-tickl,ng*zoom, tickl, ng*zoom);
    textAlign(RIGHT, TOP); textSize(10);
    text(-ng, -2, ng*zoom);
  }  

  // blue sinusoid
  noFill();
  stroke(70,70,255); strokeWeight(4);
  float R=4;
  float nt = 100;
  float dt = 2*PI/(nt-1);
  beginShape();
  for(int n=0; n<nt; ++n)
  {
    
    float x=2*R*zoom*sin(n*dt+2*phase);
    float y=R*zoom*cos(3*n*dt+phase);
    vertex(x,y);
    
    ellipse(x,y,3,3);
  }
  endShape();
  
  // blue border
  translate(-width/2, -height/2);
  stroke(70,70,255); noFill();
  rect(0,0,width-1,height-1);  
  
  textAlign(LEFT, TOP); textSize(10);
  text("fps: "+round(frameRate), 10,10);
  
  // increment phase
  phase+=dphase;
}

void keyPressed()
{
  
  if (key == 's')
  {
    println("saving screenshot.png");
    save("screenshot.png");
  }
  else if (key == CODED)
  {
    if(keyCode==LEFT)
      dphase+=0.001;
    if(keyCode==RIGHT)
      dphase-=0.001;
  
  }
  else
  {
    println("an unknown key has been pressed");
  }
}