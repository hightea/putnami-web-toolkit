/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.sample.domain;

import java.io.Serializable;
import java.util.List;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

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
		return members == null ? 0 : members.size();
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
