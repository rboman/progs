
// passage (x,y) a (x_ecran,y_ecran) :
//
// x_e = -xmin*zox + x*zox
// y_e =  ymax*zoy - y*zoy
//

import java.awt.*;
import java.applet.*;
import java.lang.*;
import javax.swing.event.*;
import java.awt.event.*;

public class PlotCanevas extends Canvas implements MouseListener, MouseMotionListener {
    public boolean show_grid = true;
    public boolean show_axis = true;
    public boolean show_label = true;

    protected int x1, y1, x2, y2;
    protected double xx = 0.0, yy = 0.0;
    protected double xmin, xmax, ymin, ymax;
    protected double gdx, gdy;
    protected double zox, zoy;
    protected ExpMath func = null;
    protected boolean verbose = false; // infos de debug vers stdout

    public PlotCanevas(String f, char varx, double valx) {
        restore();
        addMouseListener(this);
        addMouseMotionListener(this);

        func = new ExpMath(f, varx, valx);
    }

    public void restore() {
        xmin = -10.0;
        xmax = 10.0;
        ymin = -10.0;
        ymax = 10.0;
        gdx = 2.0;
        gdy = 2.0;
    }

    public void paint(Graphics g) {
        this.setBackground(Color.white);
        Rectangle r = this.getBounds();
        zox = (double) r.width;
        zoy = (double) r.height;
        zox = zox / (xmax - xmin);
        zoy = zoy / (ymax - ymin);

        // Trace la grille
        Color grid_color = new Color(200, 200, 255);
        g.setColor(grid_color);
        if (show_grid) {
            for (int i = (int) (xmin / gdx); i < (int) (xmax / gdx); i++) {
                int va = (int) (-xmin * zox + i * gdx * zox);
                g.drawLine(va, 0, va, r.height);
            }
            for (int i = (int) (ymin / gdy); i < (int) (ymax / gdy); i++) {
                int va = (int) (ymax * zoy - i * gdy * zoy);
                g.drawLine(0, va, r.width, va);
            }
        }

        // Trace les axes
        Color axis_color = new Color(100, 100, 255);
        g.setColor(axis_color);
        if (show_axis) {
            for (int i = (int) (xmin / gdx); i < (int) (xmax / gdx); i++) {
                int va = (int) (-xmin * zox + i * gdx * zox);
                int va2 = (int) (ymax * zoy);
                g.drawLine(va, va2 - 2, va, va2 + 2);
                if (show_label)
                    g.drawString(Double.toString(i * gdx), va + 2, va2 - 2);
            }
            for (int i = (int) (ymin / gdy); i < (int) (ymax / gdy); i++) {
                int va = (int) (ymax * zoy - i * gdy * zoy);
                int va2 = (int) (-xmin * zox);
                g.drawLine(va2 - 2, va, va2 + 2, va);
                if (show_label)
                    if (i != 0)
                        g.drawString(Double.toString(i * gdy), va2 + 3, va);
            }
            g.drawLine((int) (-xmin * zox), 0, (int) (-xmin * zox), r.height);
            g.drawLine(0, (int) (ymax * zoy), r.width, (int) (ymax * zoy));
        }

        // Trace la fonction

        g.setColor(new Color(200, 100, 100));

        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        g2d.setStroke(new BasicStroke(2));
        if (func == null)
            func = new ExpMath("sin(x)", 'x', 0);

        int x_old = 0, y_old = 0;
        for (int i = 0; i < r.width; i++) {
            double x = xmin + ((double) i) / zox;
            double y = Funct(x);
            x = (-xmin + x) * zox;
            y = (-y + ymax) * zoy;
            if (i != 0)
                g.drawLine(i - 1, y_old, i, (int) y);
            x_old = (int) x;
            y_old = (int) y;
        }

        // Trace un cadre externe
        g2d.setStroke(new BasicStroke(1));
        g.setColor(new Color(100, 100, 255));
        g.drawRect(0, 0, r.width - 1, r.height - 1);
    }

    protected double Funct(double x) {
        func.put(x);
        return func.parse();
    }

    public void mousePressed(MouseEvent e) {
        if (verbose)
            System.out.println("mousePressed");
        Point pt = e.getPoint();
        x1 = pt.x;
        x2 = pt.x;
        y1 = pt.y;
        y2 = pt.y;
    }

    public void mouseReleased(MouseEvent e) {
        if (verbose)
            System.out.println("mouseReleased");
        double tx1, tx2, ty1, ty2;

        Point pt = e.getPoint();

        x2 = pt.x;
        y2 = pt.y;
        if ((x2 == x1) && (y2 == y1)) {
            tx1 = (xmax - xmin) / 2.0;
            ty1 = (ymax - ymin) / 2.0;
            xmin = xmin - tx1;
            xmax = xmax + tx1;
            ymin = ymin - ty1;
            ymax = ymax + ty1;
        } else {
            tx1 = (double) x1 / zox + xmin;
            tx2 = (double) x2 / zox + xmin;
            ty1 = -1 * (double) y1 / zoy + ymax;
            ty2 = -1 * (double) y2 / zoy + ymax;

            xmin = tx1;
            xmax = tx2;
            ymin = ty2;
            ymax = ty1;
            System.out.println("xmin =" + xmin + ", xmax=" + xmax);
            System.out.println("ymin =" + ymin + ", ymax=" + ymax);
        }

        if (xmin > xmax) {
            tx1 = xmin;
            xmin = xmax;
            xmax = tx1;
        }
        if (ymin > ymax) {
            tx1 = ymin;
            ymin = ymax;
            ymax = tx1;
        }

        this.repaint();
    }

    public void mouseEntered(MouseEvent e) {
        if (verbose)
            System.out.println("mouseEntered");
    }

    public void mouseExited(MouseEvent e) {
        if (verbose)
            System.out.println("mouseExited");
    }

    public void mouseClicked(MouseEvent e) {
        if (verbose)
            System.out.println("mouseClicked");
    }

    public void mouseMoved(MouseEvent e) {
        if (verbose)
            System.out.println("mouseMoved");
        Point pt = e.getPoint();
        xx = pt.x / zox + xmin;
        yy = -pt.y / zoy + ymax;
    }

    public void mouseDragged(MouseEvent e) {
        if (verbose)
            System.out.println("mouseDragged");
        Point pt = e.getPoint();
        xx = pt.x / zox + xmin;
        yy = -pt.y / zoy + ymax;

        Graphics g = this.getGraphics();
        g.setXORMode(Color.red);
        g.drawRect(x1, y1, x2 - x1, y2 - y1);
        g.drawRect(x1, y1, pt.x - x1, pt.y - y1);
        x2 = pt.x;
        y2 = pt.y;
    }
}
