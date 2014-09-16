package fr.putnami.pwt.tutorial.server.service;

import org.springframework.stereotype.Service;

import fr.putnami.pwt.tutorial.shared.domain.Contact;
import fr.putnami.pwt.tutorial.shared.service.ContactService;

@Service
public class ContactServiceImpl implements ContactService {

	@Override
	public void sendContact(Contact contact) {
		// Do Something
		System.out.println("Contact received : " + contact.toString());
	}
}
