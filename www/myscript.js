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
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/light%20grey.PNG?raw=true" >&nbsp;Light Grey</td>
            <td>Carbon</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/red.PNG?raw=true">&nbsp;Red</td>
            <td>Oxygen</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/white.PNG?raw=true" style = "border: .75px solid black;">&nbsp;White</td>
            <td>Hydrogen</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/light%20blue.PNG?raw=true">&nbsp;Light Blue</td>
            <td>Nitrogen</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/yellow.PNG?raw=true">&nbsp;Yellow</td>
            <td>Sulfur</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/orange.PNG?raw=true">&nbsp;Orange</td>
            <td>Phosphorous</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/green.PNG?raw=true">&nbsp;Green</td>
            <td>Chlorine</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/brown.PNG?raw=true">&nbsp;Brown</td>
            <td>Bromine, Zinc</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/blue.PNG?raw=true">&nbsp;Blue</td>
            <td>Sodium</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/dark%20orange.PNG?raw=true">&nbsp;Dark Orange</td>
            <td>Iron</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/dark%20green.PNG?raw=true">&nbsp;Dark Green</td>
            <td>Magnesium</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/dark%20grey.PNG?raw=true">&nbsp;Dark Grey</td>
            <td>Calcium</td>
        </tr>
        <tr>
            <td><img class = "cpk" src = "https://github.com/czhang2718/GSRS-Dashboard/blob/master/cpk%20colors/deep%20pink.PNG?raw=true">&nbsp;Deep Pink</td>
            <td>Unknown</td>
        </tr>
    </table>`;
    }
    lastClicked = id;
}
