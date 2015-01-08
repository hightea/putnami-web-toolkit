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
package fr.putnami.pwt.core.widget.shared.assist;

import com.google.common.base.Objects;

import java.io.Serializable;

import fr.putnami.pwt.core.widget.client.assist.SimpleOracle;

public class SimpleSuggestion<T> implements Oracle.Suggestion<T>, Serializable {

	private T value;
	private int relevance;

	public SimpleSuggestion() {
	}

	public SimpleSuggestion(T value, int relevance) {
		this.value = value;
		this.relevance = relevance;
  }

	@Override
	public T getValue() {
		return value;
	}

	public void setValue(T value) {
		this.value = value;
	}

	@Override
	public int getRelevance() {
		return relevance;
	}

	public void setRelevance(int relevance) {
		this.relevance = relevance;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(value);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof SimpleOracle) {
			return Objects.equal(value, value);
		}
		return false;
	}

}
