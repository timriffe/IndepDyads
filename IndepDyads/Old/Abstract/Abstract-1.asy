if(!settings.multipleView) settings.batchView=false;
settings.tex="pdflatex";
settings.inlinetex=true;
deletepreamble();
defaultfilename="Abstract-1";
if(settings.render < 0) settings.render=4;
settings.outformat="";
settings.inlineimage=true;
settings.embed=true;
settings.toolbar=false;
viewportmargin=(2,2);

// DepViewAxes produced by rgl
settings.prc = true;
size(3inches, 3inches);
import graph3;
currentprojection = orthographic(0, -3.101144, 1.128724, up = (0, 0.3420201, 0.9396926));
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
draw((0.4985029, 0, -0.1762474)
--(-0.2492514, 0.4317162, -0.1762474)
);
label("P", position = (-0.002741766, 0.3181748, -0.1938721), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.8235294, 0.7372549, 0.1764706);
draw((0.4985029, 0, -0.1762474)
--(-0.2492514, -0.4317162, -0.1762474)
);
label("C", position = (-0.002741766, -0.3181748, -0.1938721), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.5333334, 0.1215686, 0.5764706);
draw((0.4985029, 0, -0.1762474)
--(0, 0, 0.5287422)
);
label("D", position = (0.1809565, 0, 0.3257052), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.8235294, 0.2156863, 0.2156863);
draw((-0.2492514, 0.4317162, -0.1762474)
--(-0.2492514, -0.4317162, -0.1762474)
);
label("A", position = (-0.2741766, -0.1614619, -0.1938721), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.3058824, 0.7882353, 0.2313726);
draw((-0.2492514, 0.4317162, -0.1762474)
--(0, 0, 0.5287422)
);
label("T", position = (-0.09047827, 0.156713, 0.3257052), align = (0,0));
currentpen = colorless(currentpen) + rgb(0.772549, 0.4588235, 0.1686275);
draw((-0.2492514, -0.4317162, -0.1762474)
--(0, 0, 0.5287422)
);
label("L", position = (-0.09047827, -0.156713, 0.3257052), align = (0,0));
currentpen += linewidth(2);
currentpen = colorless(currentpen) + rgb(0, 1, 1);
draw((0, 0, 0.6873648)
--(0, 0, -0.2291216)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("APC", position = (0, 0, -0.2291216), align = (0,0));
currentpen = colorless(currentpen) + rgb(0, 1, 1);
draw((-0.3240269, -0.561231, -0.2291216)
--(0.108009, 0.187077, 0.07637387)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("TPD", position = (0.108009, 0.187077, 0.07637387), align = (0,0));
currentpen = colorless(currentpen) + rgb(0, 1, 1);
draw((-0.3240269, 0.561231, -0.2291216)
--(0.108009, -0.187077, 0.07637387)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("CDL", position = (0.108009, -0.187077, 0.07637387), align = (0,0));
currentpen = colorless(currentpen) + rgb(0, 1, 1);
draw((0.6480538, 0, -0.2291216)
--(-0.2160179, 0, 0.07637387)
);
currentpen = colorless(currentpen) + rgb(0, 0, 0);
label("TAL", position = (-0.2160179, 0, 0.07637387), align = (0,0));
currentlight.background = rgb(0.2980392, 0.2980392, 0.2980392);
currentlight.background = rgb(1, 1, 1);
currentlight.background = rgb(1, 1, 1);
