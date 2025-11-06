function toggleOpenDropdown() {
    const dropdownId = "solutionsDropdown"
    const dropdown = document.getElementById(dropdownId);
    console.log("fired");
    if (dropdown) {
        if (dropdown.classList.contains('block')) {
            // If open, close
	    dropdown.classList.add('hidden');
            dropdown.classList.remove('block');	    
        } else {
            // If closed, open 
            dropdown.classList.add('block');
	    dropdown.classList.remove('hidden');
        }  
    }
}
    
function scrollToTop() {}


function submitEmail() {
    const inputValue = document.getElementById('emailInput').value;
    const xhr = new XMLHttpRequest();
    xhr.open('POST', '/emailMe', true);
    xhr.setRequestHeader('Content-Type', 'application/json;charset=UTF-8');
    // Set up a callback to handle the response
    xhr.onload = function() {
        if (xhr.status >= 200 && xhr.status < 300) {
            console.log( 'Response: ' + xhr.responseText) ;
        } else {
            console.error('Error: ' + xhr.status);
        }
    };
    xhr.send(JSON.stringify(inputValue));
}


// Could we have a quasi quoter which just ensures valid names
function toggleFaq(containerId, imageId, borderId, openImg, openBorder, openContainer, closedImg, closedBorder, closedContainer) {
    
    console.log(arguments)
    const isOpen = document.getElementById(containerId).classList.contains('hidden')
    let img, containerClasses, borderClasses;
    if (!isOpen) {
	// change to
	img = closedImg
	containerClasses = closedContainer
	borderClasses = closedBorder 
    }
    else {
	// change to
	img = openImg
	containerClasses = openContainer
	borderClasses = openBorder 
    }
    console.log(document.getElementById(imageId))
    console.log(document.getElementById(containerId))
    console.log(document.getElementById(borderId))
    
    document.getElementById(imageId).src = img
    document.getElementById(containerId).className = containerClasses
    document.getElementById(borderId).className = borderClasses
    console.log('got here')
}

// TODO: toggleSetChild

function setChild(parentId, childHTML) {
    console.log(childHTML)
    document.getElementById(parentId).innerHTML = childHTML 
}

function setChildToggle_(parentId, tag, childHTML) {
    const parentElement = document.getElementById(parentId);
    if (parentElement.innerHTML === childHTML) {
	console.log("===")
	console.log(parentElement)
	parentElement.innerHTML = "";
	return 
    }
    else {
	console.log("=/=")
	// parentElement.innerHTML = childHTML;
	return
    }
}

function setChildToggle(parentId, name, childHTML) {
    const parentElement = document.getElementById(parentId)
    const maybeExistingChild = parentElement.querySelector('span[data-name]');
    if (maybeExistingChild) {
	if (maybeExistingChild.getAttribute('data-name') == name) {
	    console.log('found match')
	    parentElement.innerHTML = '';
	} else {
	    console.log('child but no match')
	    console.log(maybeExistingChild.getAttribute('data-name'))
	    parentElement.innerHTML = '';
            createNamedSpan(parentElement, name, childHTML);
	}
    } else {
	console.log('is empty')
	parentElement.innerHTML = '';
	createNamedSpan(parentElement, name, childHTML);
    }
}


function createNamedSpan(parent, name, childHTML) {
    const newChild = document.createElement('span');
    newChild.setAttribute('data-name', name);
    newChild.innerHTML = childHTML;
    parent.appendChild(newChild);
}

// TODO
// function setChildNoPropogate(parentId, childHTML) {
//     console.log(childHTML)
//     document.getElementById(parentId).innerHTML = childHTML 
// }
