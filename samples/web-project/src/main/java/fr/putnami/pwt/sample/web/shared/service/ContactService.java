package fr.putnami.pwt.sample.web.shared.service;

import java.util.List;

import fr.putnami.pwt.sample.web.shared.domain.Contact;
import fr.putnami.pwt.sample.web.shared.domain.Group;

public interface ContactService {

	List<Group> getGroups();

	List<Contact> getPeople();

	Contact getPerson(String name);

	Contact savePerson(Contact person);
}
