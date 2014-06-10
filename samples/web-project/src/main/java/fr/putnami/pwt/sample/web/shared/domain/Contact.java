package fr.putnami.pwt.sample.web.shared.domain;

import java.util.List;

import com.google.common.collect.Lists;

public class Contact extends Person {

	private List<String> emails;
	private List<String> phones;
	private List<String> groups = Lists.newArrayList();
	private String remarks;

	public List<String> getEmails() {
		return emails;
	}

	public void setEmails(List<String> emails) {
		this.emails = emails;
	}

	public List<String> getPhones() {
		return phones;
	}

	public void setPhones(List<String> phones) {
		this.phones = phones;
	}

	public List<String> getGroups() {
		return groups;
	}

	public void setGroups(List<String> groups) {
		this.groups = groups;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

}
