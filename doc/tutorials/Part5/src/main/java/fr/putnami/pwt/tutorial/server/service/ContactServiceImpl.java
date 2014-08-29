package fr.putnami.pwt.tutorial.server.service;

import fr.putnami.pwt.tutorial.shared.domain.Contact;
import fr.putnami.pwt.tutorial.shared.service.ContactService;

public class ContactServiceImpl implements ContactService {

	@Override
	public void sendContact(Contact contact) {
		// Do Something
		System.out.println("Contact received : " + contact.toString());
	}
}
