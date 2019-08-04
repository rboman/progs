//
// $Id$
//
// RoBo 20.08.99
// traduction Java 6 (30-05-2010)

// "appletviewer PlotApplet.java" works because of the following line.
// <applet code="PlotApplet.class" height="400" width="640"></applet>

import java.awt.*;
import java.applet.*;
import java.awt.event.*;

public class PlotApplet extends Applet implements ActionListener, ItemListener {
    protected Button but1 = null;
    protected Button but2 = null;

    protected PlotCanevas plot = null;
    protected Checkbox chk1 = null, chk2 = null, chk3 = null;
    protected TextField tf1 = null, tf3 = null, tf4 = null;
    protected Label lab1 = null, lab3 = null, lab4 = null;
    protected ExpMath expmath = null;
    protected String defaultf = "5*sin(x)+cos(4*x)";

    public void init() {
        this.setLayout(new BorderLayout(5, 5));

        but1 = new Button("Applet EquPlot - RoBo 20.08.99");
        but1.addActionListener(this);
        but2 = new Button("Bouton 2");
        plot = new PlotCanevas(defaultf, 'x', 0);
        this.add("North", but1);
        // this.add("South",but2);
        this.add("Center", plot);

        chk1 = new Checkbox("Show Grid");
        chk1.addItemListener(this);
        chk2 = new Checkbox("Show Axis");
        chk2.addItemListener(this);
        chk3 = new Checkbox("Show Labels");
        chk3.addItemListener(this);
        Panel p1 = new Panel();
        p1.setLayout(new GridLayout(5, 1, 5, 5));
        p1.add("test1", chk1);
        p1.add("test2", chk2);
        p1.add("test3", chk3);

        chk1.setState(plot.show_grid);
        chk2.setState(plot.show_axis);
        chk3.setState(plot.show_label);

        // Param grid

        lab3 = new Label("dx =");
        tf3 = new TextField(Double.toString(plot.gdx), 4);
        tf3.addActionListener(this);
        Panel p3 = new Panel();
        p3.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
        p3.add(lab3);
        p3.add(tf3);
        p1.add("test4", p3);

        lab4 = new Label("dy =");
        tf4 = new TextField(Double.toString(plot.gdy), 4);
        tf4.addActionListener(this);
        Panel p4 = new Panel();
        p4.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
        p4.add(lab4);
        p4.add(tf4);
        p1.add("test5", p4);

        this.add("East", p1);

        // Zone fonction

        lab1 = new Label("Function :   y(x) =");
        tf1 = new TextField(defaultf, 40);
        tf1.addActionListener(this);

        Panel p2 = new Panel();
        p2.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
        p2.add(lab1);
        p2.add(tf1);
        this.add("South", p2);
    }

    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        if (source == tf1) {
            plot.func = new ExpMath(tf1.getText(), 'x', 0);
            plot.repaint();
        } else if (source == tf3) {
            plot.gdx = (Double.valueOf(tf3.getText())).doubleValue();
            plot.repaint();
        } else if (source == tf4) {
            plot.gdy = (Double.valueOf(tf4.getText())).doubleValue();
            plot.repaint();
        } else if (source == but1) {
            plot.restore();
            plot.repaint();
        }
    }

    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        if (source == chk1) {
            if (plot.show_grid == true)
                plot.show_grid = false;
            else
                plot.show_grid = true;
            plot.repaint();

        } else if (source == chk2) {
            if (plot.show_axis == true)
                plot.show_axis = false;
            else
                plot.show_axis = true;
            plot.repaint();

        } else if (source == chk3) {
            if (plot.show_label == true)
                plot.show_label = false;
            else
                plot.show_label = true;
            plot.repaint();
        }
    }

}
