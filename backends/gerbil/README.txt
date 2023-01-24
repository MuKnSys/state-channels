1) Configuring for Gerbil

For any Gerbil module:
=> gerbil.pkg must contain the module's description ;
=> GERBIL_LOADPATH should be set to the path containing gerbil.pkg ;

Then, gxi -e '(import :<my-module-path>/<my-prog>)' works.

I don't know if the value of GERBIL_LOADPATH can be a list of paths,
thus enabling loading several programs/libraries from different modules.
