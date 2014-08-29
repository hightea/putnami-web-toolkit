package fr.putnami.pwt.tutorial.shared.domain;

import java.io.Serializable;
import java.util.Date;

import com.google.common.base.Objects;

public class Contact implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;
	private String email;
	private Date birthday;

	private Integer tutorialNote = Integer.valueOf(5); // Default Value

	private String subject = "About PWT"; // Default Value
	private String message;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public Date getBirthday() {
		return birthday;
	}

	public void setBirthday(Date birthday) {
		this.birthday = birthday;
	}

	public Integer getTutorialNote() {
		return tutorialNote;
	}

	public void setTutorialNote(Integer tutorialNote) {
		this.tutorialNote = tutorialNote;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this)
				.add("Name", name)
				.add("Email", email)
				.add("Birthday", birthday)
				.add("Tutorial Note", tutorialNote)
				.add("Subject", subject)
				.add("Message", message).toString();
	}
}
