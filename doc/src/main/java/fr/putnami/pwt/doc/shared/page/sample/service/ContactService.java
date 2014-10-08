/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.shared.page.sample.service;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Random;

import fr.putnami.pwt.doc.shared.page.sample.domain.Address;
import fr.putnami.pwt.doc.shared.page.sample.domain.Contact;
import fr.putnami.pwt.doc.shared.page.sample.domain.Gender;
import fr.putnami.pwt.doc.shared.page.sample.domain.Group;
import fr.putnami.pwt.doc.shared.page.sample.domain.Person;

public final class ContactService {

	private static ContactService INSTANCE;

	public static ContactService get() {
		if (ContactService.INSTANCE == null) {
			ContactService.INSTANCE = new ContactService();
		}
		return ContactService.INSTANCE;
	}

	private static final String[] FEMALE_FIRST_NAMES = {
		"Mary", "Patricia", "Linda", "Barbara", "Elizabeth", "Jennifer", "Maria", "Susan",
		"Margaret", "Dorothy", "Lisa", "Nancy", "Karen", "Betty", "Helen", "Sandra", "Donna",
		"Carol", "Ruth", "Sharon", "Michelle", "Laura", "Sarah", "Kimberly", "Deborah", "Jessica",
		"Shirley", "Cynthia", "Angela", "Melissa", "Brenda", "Amy", "Anna", "Rebecca", "Virginia",
		"Ana", "Renee", "Ida", "Vivian", "Roberta", "Holly", "Brittany", "Melanie", "Loretta",
		"Yolanda", "Jeanette", "Laurie", "Katie", "Kristen", "Vanessa", "Alma", "Sue", "Elsie",
		"Beth", "Jeanne"};
	private static final String[] MALE_FIRST_NAMES = {
		"James", "John", "Robert", "Michael", "William", "David", "Richard", "Charles", "Joseph",
		"Thomas", "Christopher", "Daniel", "Paul", "Mark", "Donald", "George", "Kenneth", "Steven",
		"Edward", "Brian", "Ronald", "Anthony", "Kevin", "Jason", "Matthew", "Gary", "Timothy",
		"Jose", "Larry", "Jeffrey", "Frank", "Scott", "Eric", "Stephen", "Andrew", "Raymond",
		"Alvin", "Tim", "Wesley", "Gordon", "Dean", "Greg", "Jorge", "Dustin", "Pedro", "Derrick",
		"Dan", "Lewis", "Zachary", "Corey", "Herman", "Maurice", "Vernon", "Roberto", "Clyde",
		"Glen", "Hector", "Shane", "Ricardo", "Sam", "Rick", "Lester", "Brent", "Ramon", "Charlie",
		"Tyler", "Gilbert", "Gene"};
	private static final String[] LAST_NAMES = {
		"Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller", "Wilson", "Moore",
		"Taylor", "Anderson", "Thomas", "Jackson", "White", "Harris", "Martin", "Thompson", "Garcia",
		"Perkins", "Hudson", "Spencer", "Gardner", "Stephens", "Payne", "Pierce", "Berry",
		"Matthews", "Arnold", "Wagner", "Willis", "Ray", "Watkins", "Olson", "Carroll", "Duncan",
		"Snyder", "Hart", "Cunningham", "Bradley", "Lane", "Andrews", "Ruiz", "Harper", "Fox",
		"Riley", "Armstrong", "Carpenter", "Weaver", "Greene", "Lawrence", "Elliott", "Chavez",
		"Sims", "Austin", "Peters", "Kelley", "Franklin", "Lawson"};
	private static final String[] STREET_NAMES =
	{
		"Peachtree", "First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Tenth",
		"Fourteenth", "Spring", "Techwood", "West Peachtree", "Juniper", "Cypress", "Fowler",
		"Piedmont", "Juniper", "Main", "Central", "Currier", "Courtland", "Williams",
		"Centennial", "Olympic", "Baker", "Highland", "Pryor", "Decatur", "Bell", "Edgewood",
		"Mitchell", "Forsyth", "Capital"};
	private static final String[] STREET_SUFFIX = {
		"St", "Rd", "Ln", "Blvd", "Way", "Pkwy", "Cir", "Ave"};
	private static final String[] CITIES = {
		"New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia", "San Antonio",
		"San Diego"};
	private static final String[] DOMAINS = {
		"gmail.com", "google.com", "yahoo.com", "aol.com", "msn.com", "comcast.net", "cox.net",
		"sbcgloval.net"};

	private static long SEQUENCE = 0L;

	private final List<Contact> contacts = Lists.newArrayList();
	private final List<Group> groups = Lists.newArrayList();
	private final Group groupAll = new Group("All");

	private ContactService() {
		this.groups.add(new Group("Familly"));
		this.groups.add(new Group("Friend"));
		this.groups.add(new Group("Colleague"));
		for (int i = 0; i < 50; i++) {
			this.savePerson(this.createRandom());
		}
		this.groups.add(0, this.groupAll);
		Collections.sort(this.groupAll.getMembers());
	}

	public List<Group> getGroups() {
		return Lists.newArrayList(this.groups);
	}

	public List<Contact> getPeople() {
		return Lists.newArrayList(this.contacts);
	}

	public Contact getPerson(final String name) {
		return Iterables.find(this.contacts, new Predicate<Person>() {

			@Override
			public boolean apply(Person input) {
				return name != null && name.equals(input.getName());
			}
		}, null);
	}

	public Contact savePerson(Contact contact) {
		if (contact.isNew()) {
			contact.setId(++ContactService.SEQUENCE);
		}
		this.contacts.remove(contact);
		this.contacts.add(contact);
		Collections.sort(this.contacts);

		this.groupAll.getMembers().remove(contact);
		for (Group group : this.groups) {
			group.getMembers().remove(contact);
			if (contact.getGroups().contains(group.getName())) {
				group.getMembers().add(contact);
			}
			Collections.sort(group.getMembers());
		}
		this.groupAll.getMembers().add(contact);
		Collections.sort(this.groupAll.getMembers());
		return contact;
	}

	private Contact createRandom() {
		Contact contact = new Contact();
		if (new Random().nextBoolean()) {
			contact.setName(this.nextValue(ContactService.LAST_NAMES) + " "
				+ this.nextValue(ContactService.MALE_FIRST_NAMES));
			contact.setGender(Gender.MALE);
		} else {
			contact.setName(this.nextValue(ContactService.LAST_NAMES) + " "
				+ this.nextValue(ContactService.FEMALE_FIRST_NAMES));
			contact.setGender(Gender.FEMALE);
		}

		// Create a birthday between 20-80 years ago.
		int year = new Date().getYear() - 21 - new Random().nextInt(61);
		contact.setBirthday(new Date(year, new Random().nextInt(12), 1 + new Random().nextInt(31)));

		// Create a weight between 30 and 120
		contact.setWeight(30 + new Random().nextInt(90));

		// Create an address.
		Address address = new Address();
		address.setStreet(this.nextValue(ContactService.STREET_NAMES));
		address.setPostCode(this.nextValue(ContactService.STREET_SUFFIX));
		address.setCity(this.nextValue(ContactService.CITIES));
		contact.setAddress(address);

		contact.setEmails(Lists.<String> newArrayList());
		contact.getEmails().add(
			contact.getName().replace(" ", ".").toLowerCase() + "@"
				+ this.nextValue(ContactService.DOMAINS));

		Group memberOf = this.groups.get(new Random().nextInt(this.groups.size()));
		memberOf.getMembers().add(contact);
		this.groupAll.getMembers().add(contact);

		contact.setGroups(Lists.<String> newArrayList());
		contact.getGroups().add(memberOf.getName());

		return contact;
	}

	private <T> T nextValue(T[] array) {
		return array[new Random().nextInt(array.length)];
	}
}
