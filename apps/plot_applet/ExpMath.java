// ---------------------------------------------------------
// ExpMath : classe d'evaluation d'expressions mathematiques
//           
// RoBo : conversion C->java le 20.08.99
// ---------------------------------------------------------
//
// constructeur :
//
//    public ExpMath(String expr, char varx, double valx)
//
// fct publique :
//
//    public double parse()      : evalue la chaine en 'x'
//    public void put(double x)  : pose 'x'=x
//
// ---------------------------------------------------------

import java.lang.*;

public class ExpMath 
{
   // Variables
   // =========

   protected String expr;   // expression sous forme de chaine
   protected int expr_l;    // longueur de la chaine
   protected int pos;       // pointeur d'evaluation
   protected char varx;     // variable indep.
   protected double valx;   // valeur de la variable

   // Constructeur
   // ============

   public ExpMath(String expr, char varx, double valx)
   {
      this.expr=expr;
      this.expr_l = expr.length();
      this.varx = varx;
      this.valx = valx;
      // System.out.println("Nouvel Objet : " + expr + ", long =" + expr_l);
   }

   // Evaluation de la chaine
   // =======================

   public double parse()
   {
      pos=0;
      return this.eval();
   }

   // Assignation de la valeur de la variable
   // =======================================

   public void put(double x)
   {
      this.valx=x;
   }

   // Conversion Chaine -> double
   // ==========================

   protected double getnum()
   {
      StringBuffer buf = new StringBuffer(30);
      char t;
      double val=0;
      int i=pos;
      boolean expon=false;

      while(true)
      {
         if(pos>=expr_l) break;
         t=expr.charAt(pos);
         if(((t>='0')&&(t<='9'))||(t=='.'))
            { buf.append(t);  pos++; expon=false; }
         else if (t=='e') {expon=true; buf.append(t);  pos++;}
         else if ((t=='-')&&(expon==true)) {buf.append(t);  pos++;}
         else break;
      }
      val= (Double.valueOf(buf.toString())).doubleValue();
      //  System.out.println("getnum : val ="+val);
      pos--; return val;
   }

   // Lecture d'un terme (+ ou -)
   // ===========================

   protected double terme()
   {
      double val=0, tmp=0;
      char t;
      boolean alp=false, moins=false;

      while(true)
      {
         if(pos>=expr_l) return val;
         t=expr.charAt(pos);
         if(t==varx)
            { val=valx; alp=true; if(moins==true) val=-val;}
         else if(((t>='0')&&(t<='9'))||(t=='.'))
            { val=getnum(); alp=true; if(moins==true) val=-val;}
         else if(t=='-')
            if(alp==true) return val; else moins=true;
         else if((t=='+')&&(alp==true)) return val;
         else if(t=='/')
         {
            tmp=facteur();
            if(tmp==0) 
               System.out.println("Division par ZERO !");
            else
               val/=tmp;
         }
         else if(t=='*') 
         {
            // System.out.println("terme1 * : val ="+val);
            val*=facteur();
            // System.out.println("terme2 * : val ="+val);
         }
         else if(t==')') return val;
         else if(t=='(') { pos++; val=eval(); alp=true; if(moins==true) val=-val;} 
         else if(expr.regionMatches(true,pos,"sin(",0,4))
         {

            pos+=4; 
            tmp=eval(); 
            // System.out.println("arg du sin : tmp ="+tmp);
            val=(double)Math.sin(tmp);
            alp=true; if(moins==true) val=-val;
         }
         else if(expr.regionMatches(true,pos,"cos(",0,4))
         {

            pos+=4; 
            tmp=eval(); 
            // System.out.println("arg du cos : tmp ="+tmp);
            val=(double)Math.cos(tmp);
            alp=true; if(moins==true) val=-val;
         }

         pos++;
      }
   }

   // Lecture d'un facteur (* ou /)
   // =============================

   protected double facteur()
   {
      double val=0,tmp=0;
      char t;
      while(true)
      {
         pos++;
         if(pos>=expr_l) return val;
         t=expr.charAt(pos);
         if(t=='(') { pos++; val=eval(); return(val); }
         else if((t>='0')&&(t<='9')) { val=getnum(); return val; }
         else if(t==varx)
            { val=valx; return(val);}
         else if(expr.regionMatches(true,pos,"sin(",0,4))
         {

            pos+=4; 
            tmp=eval(); 
            // System.out.println("arg du sin : tmp ="+tmp);
            val=(double)Math.sin(tmp);
            return val;
         }
         else if(expr.regionMatches(true,pos,"cos(",0,4))
         {

            pos+=4; 
            tmp=eval(); 
            // System.out.println("arg du cos : tmp ="+tmp);
            val=(double)Math.cos(tmp);
            return val;
         }

      }
   }

   // Evaluation d'une parenthese
   // ===========================

   protected double eval()
   {
      double sum=0, x=0;

      while(pos<expr_l)
      {
         x=terme(); sum +=x;
         if(pos<expr_l)
         if(expr.charAt(pos)==')')
         {
            // System.out.println("Valeur parenthese =" + sum + " (pos="+pos+")"); 
            return sum;
         }
      }
      return sum;
   }

   // Fonction 'main' de demo
   // =======================

   public static void main(String arg[])
   {
      double res=0;
      
      if(arg.length!=1)
      {
         System.out.println("\nExpMath : classe d'evaluation"
                                      +" d'expressions mathematiques\n");
         System.out.println("usage :");
         System.out.println("  java ExpMath [expression]");
      }
      else
      {
         ExpMath a=new ExpMath(arg[0],'x',2);
         res=a.parse(); 
         System.out.println(" " + arg[0] + " = " + res);
      }
   }
}


