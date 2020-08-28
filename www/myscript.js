let col_1a = false;
let col_1b = true;
let col_1c = true;
let lastClicked = ""; //prevent rerendering

function shade(id){
    console.log(document.getElementsByClassName("button-obj")[0]);
    let buttons = document.getElementsByClassName("button-obj");
    for(let i=0; i<3; i++){
        buttons[i].style.background = '#F4F4F4';
    }
    document.getElementById(id).style.background = '#E7E7E7';
}

function display_mol(id, cid) {
    shade(id);
    if(lastClicked != id){
        document.getElementById("img").innerHTML = "<iframe src='https://embed.molview.org/v1/?mode=balls&bg=white&cid=".concat(
            cid, "', height=250, width=550, frameborder=0, align='middle'>");
    }
    lastClicked = id;
}       

function display_smiles(id, name){
    shade(id);
    if(lastClicked != id){
        document.getElementById("img").innerHTML = "<img src='https://cactus.nci.nih.gov/chemical/structure/".concat(
            name, "/image'>");
    }
    lastClicked = id;
}

function display_cpk(id){
    shade(id);
    if(lastClicked != id){
        document.getElementById("img").innerHTML = 
        `<p style = "margin: 15px 0 0 0;">For a comprehensive list of CPK color codes, visit <a href = "http://jmol.sourceforge.net/jscolors/">Jmol</a>.</p>
        <table class = "content-table" id = "cpk_table">
        <thead>
            <tr>
                <th>Color</th> 
                <th>Element</th>
            </tr>
        </thead>
        <tr>
            <td><div class='square' style='background-color:#C8C8C8'></div>&nbsp;Light Grey</td>
            <td>Carbon</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#F00000'></div>&nbsp;Red</td>
            <td>Oxygen</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#ffffff' border: 1px></div>&nbsp;White</td>
            <td>Hydrogen</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#8F8FFF'></div>&nbsp;Light Blue</td>
            <td>Nitrogen</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#FFC832'></div>&nbsp;Yellow</td>
            <td>Sulfur</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#FFA500'></div>&nbsp;Orange</td>
            <td>Phosphorous</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#00FF00'></div>nbsp;Green</td>
            <td>Chlorine</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#A52A2A'></div>&nbsp;Brown</td>
            <td>Bromine, Zinc</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#0000FF'></div>&nbsp;Blue</td>
            <td>Sodium</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#DD7700'></div>&nbsp;Dark Orange</td>
            <td>Iron</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#2A802A'></div>&nbsp;Dark Green</td>
            <td>Magnesium</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#808080'></div>&nbsp;Dark Grey</td>
            <td>Calcium</td>
        </tr>
        <tr>
            <td><div class='square' style='background-color:#FF1493'></div>&nbsp;Deep Pink</td>
            <td>Unknown</td>
        </tr>
    </table>`;
    }
    lastClicked = id;
}

setHeight = function() {
    const window_height = $(window).height();
    const header_height = $(".main-header").height();
    const boxHeight = window_height - header_height;
    $("#intro-pie").height(boxHeight/2);
    $("#aebar").height($('#table-box').height()+$('#intro-pie').height());
};

$(document).on("shiny:connected", function(event) {
    setHeight();
});

$(window).resize(function(e) {
    setHeight();
})
