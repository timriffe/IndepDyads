if(!settings.multipleView) settings.batchView=false;
settings.tex="pdflatex";
settings.inlinetex=true;
deletepreamble();
defaultfilename="Abstract-4";
if(settings.render < 0) settings.render=4;
settings.outformat="";
settings.inlineimage=true;
settings.embed=true;
settings.toolbar=false;
viewportmargin=(2,2);

// IndepViewAxes produced by rgl
settings.prc = true;
size(3inches, 3inches);
import graph3;
currentprojection = orthographic(0, -2.640817, 0.9611788, up = (0, 0.3420201, 0.9396926));
defaultpen(fontsize(14));
ticklabel RGLstrings(real[] at, string[] label)
{
return new string(real x) {
int i = search(at, x);
if (i < 0) return "";
else return label[i];
};
}

ticklabel RGLScale(real s)
{
return new string(real x) {return format(s*x);};
}
currentlight = light(ambient=new pen[] {rgb(1,1,1)},
diffuse = new pen[] {rgb(1,1,1)},
specular = new pen[] {rgb(1,1,1)},
position = new triple[] {(0,0,1)},
viewport = true);
currentpen += linewidth(4);
currentpen = colorless(currentpen) + rgb(0.1921569, 0.5686275, 0.7882353);
draw((0.5820827, 0, -0.2057973)
--(-0.2910413, 0.5040984, -0.2057973)
);
label("P", position = (-0.003201455, 0.3715205, -0.226377), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.8235294, 0.7372549, 0.1764706);
draw((0.5820827, 0, -0.2057973)
--(-0.2910413, -0.5040984, -0.2057973)
);
label("C", position = (-0.003201455, -0.3715205, -0.226377), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.5333334, 0.1215686, 0.5764706);
draw((0.5820827, 0, -0.2057973)
--(0, 0, 0.6173919)
);
label("D", position = (0.211296, 0, 0.3803134), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.8235294, 0.2156863, 0.2156863);
draw((-0.2910413, 0.5040984, -0.2057973)
--(-0.2910413, -0.5040984, -0.2057973)
);
label("A", position = (-0.3201455, -0.1885328, -0.226377), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.3058824, 0.7882353, 0.2313726);
draw((-0.2910413, 0.5040984, -0.2057973)
--(0, 0, 0.6173919)
);
label("T", position = (-0.105648, 0.1829877, 0.3803134), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.772549, 0.4588235, 0.1686275);
draw((-0.2910413, -0.5040984, -0.2057973)
--(0, 0, 0.6173919)
);
label("L", position = (-0.105648, -0.1829877, 0.3803134), align = (0,0));
currentpen += linewidth(2);
currentpen = colorless(currentpen) + rgb(1, 0, 1);
draw((-0.436562, 0, -0.308696)
--(0.436562, 0, 0.308696)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("AD", position = (-0.4656661, 0, -0.3292757), align = (0,0));
currentpen = colorless(currentpen) + rgb(1, 0, 1);
draw((-0.218281, 0.3780738, 0.308696)
--(0.218281, -0.3780738, -0.308696)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("TC", position = (-0.2328331, 0.4032787, 0.3292757), align = (0,0));
currentpen = colorless(currentpen) + rgb(1, 0, 1);
draw((-0.218281, -0.3780738, 0.308696)
--(0.218281, 0.3780738, -0.308696)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("LP", position = (-0.2328331, -0.4032787, 0.3292757), align = (0,0));
currentlight.background = rgb(0.2980392, 0.2980392, 0.2980392);
currentlight.background = rgb(1, 1, 1);
currentlight.background = rgb(1, 1, 1);
