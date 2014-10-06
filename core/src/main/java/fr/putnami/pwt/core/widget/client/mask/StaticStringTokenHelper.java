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
package fr.putnami.pwt.core.widget.client.mask;

import com.google.common.collect.Lists;

import java.util.List;

import fr.putnami.pwt.core.widget.client.mask.MaskValueBoxHelper.TokenHelper;

public class StaticStringTokenHelper extends TokenHelper {

	private final List<Character> delimiters;
	private final String value;

	public StaticStringTokenHelper(String value, boolean optional) {
		this.value = value;
		this.optional = optional;
		this.delimiters = Lists.newArrayList(value.charAt(0));
	}

	public StaticStringTokenHelper(String value, boolean optional, Character... delimiters) {
		this.value = value;
		this.optional = optional;
		this.delimiters = Lists.newArrayList(delimiters);
	}

	@Override
	protected String flush() {
		if (this.optional) {
			return this.token == null ? "" : this.token;
		}
		return this.value;
	}

	@Override
	protected void focus(boolean forward) {
		if (forward) {
			this.maskHelper.focusNext();
		} else {
			this.maskHelper.focusPrevious();
		}
	}

	@Override
	protected boolean handleKeyDown(int keyDown) {
		return this.value.equals(this.token);
	}

	@Override
	protected boolean handleKeyPress(char charPressed) {
		if (this.delimiters.contains(charPressed)) {
			this.token = this.value;
			return true;
		}
		return false;
	}
}
