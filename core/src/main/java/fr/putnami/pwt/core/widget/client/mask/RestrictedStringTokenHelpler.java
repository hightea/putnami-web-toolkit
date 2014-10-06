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

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.gwt.event.dom.client.KeyCodes;

import java.util.Arrays;
import java.util.List;

import fr.putnami.pwt.core.widget.client.mask.MaskValueBoxHelper.TokenHelper;

public class RestrictedStringTokenHelpler extends TokenHelper {

	private StringBuffer sb = new StringBuffer();

	private final List<String> restrictedValues;

	public RestrictedStringTokenHelpler(String... restrictedValues) {
		this.restrictedValues = Arrays.asList(restrictedValues);
	}

	@Override
	protected void focus(boolean forward) {
		this.sb = new StringBuffer();
	}

	@Override
	public void reset() {
		super.reset();
		this.sb = new StringBuffer();
	}

	@Override
	protected String flush() {
		if (this.token == null) {
			return this.restrictedValues.get(0);
		}
		return this.token;
	}

	@Override
	protected boolean handleKeyDown(int keyDown) {
		int currentIndex = 0;
		if (this.token != null) {
			currentIndex = this.restrictedValues.indexOf(this.token);
		}
		switch (keyDown) {
			case KeyCodes.KEY_DOWN:
				currentIndex += this.restrictedValues.size();
				currentIndex++;
				this.setToken(this.restrictedValues.get(currentIndex % this.restrictedValues.size()));
				break;
			case KeyCodes.KEY_UP:
				currentIndex += this.restrictedValues.size();
				currentIndex--;
				this.setToken(this.restrictedValues.get(currentIndex % this.restrictedValues.size()));
				break;
			default:
				return false;
		}
		return true;
	}

	@Override
	protected boolean handleKeyPress(char charPressed) {
		this.sb.append(charPressed);
		final String bufferedString = this.sb.toString();
		Iterable<String> filtered = Iterables.filter(this.restrictedValues, new Predicate<String>() {

			@Override
			public boolean apply(String input) {
				return input != null && input.startsWith(bufferedString);
			}
		});
		if (filtered.iterator().hasNext()) {
			this.token = filtered.iterator().next();
			return true;
		}
		return false;
	}

}