import java.awt.*;

public class MandelApp extends Panel
{
    protected float x1, y1, x2, y2;
    protected int sl, sh, nb_coul, inc, max_it, sav;
    protected Color couleurs[]= new Color[100]; 
    protected static final int RED=0;
    protected static final int BLUE=2;
    protected static final int GREEN=1;
    public int p_typ=BLUE;

    public static void main(String[] args) 
    {
        Frame f = new Frame();
        f.addWindowListener(new java.awt.event.WindowAdapter() 
        {
            public void windowClosing(java.awt.event.WindowEvent e) 
            {
                System.exit(0);
            }
        });

        int height=586; 
        int width=785;

        MandelApp ut = new MandelApp();
        ut.setSize(width, height); // same size as defined in the HTML APPLET
        f.add(ut);
        f.pack();
        ut.init();
        f.setSize(width, height + 20); // add 20, seems enough for the Frame title,
        f.setVisible(true);
    }

    public void init()
    {
        x1=-2; 
        y1=1.2f; 
        x2=1; 
        y2=-1.1f;
        sl = 500; sh = 300;
        nb_coul = 49;
        inc = 1;

        // couleurs
        for(int i=0; i<50; i++)
        {
            if(p_typ==RED)   couleurs[i]=new Color(i*4,0,0);
            if(p_typ==GREEN) couleurs[i]=new Color(0,i*4,0);
            if(p_typ==BLUE)  couleurs[i]=new Color(0,0,i*4);
        }

    }

    public void paint(Graphics g)
    {
        float a1, a2, xc, yc, xn, yn, xn1, yn1;
        int n, xe, ye;

        Rectangle r = this.getBounds();
        sl = r.width;
        sh = r.height;

        float nb_op=0;	
        max_it=0;
   
        a1=x2-x1;
        a2=y2-y1;
        for(xe=1; xe<sl-1; xe+=inc)
        {
            for(ye=1; ye<sh-1; ye+=inc)
            {
                 xc=(a1*xe)/sl+x1;
                 yc=(a2*ye)/sh+y1;

                 n=0; xn=0; yn=0;
                 while((n!=nb_coul)&&(yn*yn+xn*xn<4))
                 {
                    n+=1;
                    if(n==nb_coul-1)
                    {       
                        g.setColor(Color.black);
                        g.drawLine(xe, ye,xe,ye);
                    }
                    xn1=xn*xn-yn*yn+xc;
                    yn1=2*xn*yn+yc;
                    xn=xn1;
                    yn=yn1;
                }
                g.setColor(couleurs[n]);
                g.drawLine(xe, ye,xe,ye);
            }
        }
    }
}
