package fr.putnami.pwt.doc.server.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class PutnamiController {

	@RequestMapping(value = "/", method = RequestMethod.GET)
	public String welcomePage() {
		return "putnami";
	}

}
