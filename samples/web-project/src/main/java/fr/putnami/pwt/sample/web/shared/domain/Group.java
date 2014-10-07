package fr.putnami.pwt.sample.web.shared.domain;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

import java.io.Serializable;
import java.util.List;

public class Group implements Serializable {

	private String name;
	private int memberSize;
	private List<Contact> members = Lists.newArrayList();

	public Group() {
	}

	public Group(String name) {
		super();
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<Contact> getMembers() {
		return members;
	}

	public void setMembers(List<Contact> members) {
		this.members = members;
	}

	public int getMemberSize() {
		this.memberSize = this.members == null ? 0 : this.members.size();
		return memberSize;
	}

	public void setMemberSize(int memberSize) {
		this.memberSize = memberSize;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(name);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Group) {
			return Objects.equal(name, ((Group) obj).name);
		}
		return false;
	}

}
