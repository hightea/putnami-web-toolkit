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
package fr.putnami.pwt.doc.shared.page.sample.domain;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

import java.io.Serializable;
import java.util.List;

public class Group implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6962973768949381789L;
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
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<Contact> getMembers() {
		return this.members;
	}

	public void setMembers(List<Contact> members) {
		this.members = members;
	}

	public int getMemberSize() {
		return this.members == null ? 0 : this.members.size();
	}

	public void setMemberSize(int memberSize) {
		this.memberSize = memberSize;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(this.name);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Group) {
			return Objects.equal(this.name, ((Group) obj).name);
		}
		return false;
	}

}
