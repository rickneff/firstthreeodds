// package <none-by-intention>; 

/**
* The main entry point for the program.
*
* Could include displaying a splash screen,
* or other incidentals.
*/
public class Run
{
   public static void main(String[] args)
      throws Exception
   {
      javafx.application.Application.launch(
         generic.desktop.GUI.class, args);
   }
}
