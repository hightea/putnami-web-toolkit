/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client.mask;

import com.google.common.collect.Lists;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.i18n.client.NumberFormat;

import java.util.List;

import fr.putnami.pwt.core.widget.client.mask.MaskValueBoxHelper.TokenHelper;

public class IntegerTokenHelper extends TokenHelper {

	protected final List<Character> characters = Lists.newArrayList();

	private int defaultValue = 0;
	private int min = Integer.MIN_VALUE;
	private int max = Integer.MAX_VALUE;
	private int maxLenght = String.valueOf(Integer.MAX_VALUE).length();

	private int value;

	private NumberFormat formater = NumberFormat.getFormat("#");

	public IntegerTokenHelper() {
	}

	public IntegerTokenHelper(int defaultValue, int min, int max, int maxLenght, String format) {
		this.defaultValue = defaultValue;
		this.min = min;
		this.max = max;
		if (maxLenght != -1) {
			this.maxLenght = maxLenght;
		}
		setFormater(format);
	}

	public void setDefaultValue(int defaultValue) {
		this.defaultValue = defaultValue;
	}

	public void setMin(int min) {
		this.min = min;
	}

	public void setMax(int max) {
		this.max = max;
	}

	public int getMaxLenght() {
		return maxLenght;
	}

	public void setMaxLenght(int maxLenght) {
		this.maxLenght = maxLenght;
	}

	public void setFormater(String format) {
		this.formater = NumberFormat.getFormat(format);
	}

	@Override
	public void reset() {
		super.reset();
		characters.clear();
	}

	@Override
	public void setToken(String token) {
		super.setToken(token);
		value = defaultValue;
		if (token != null && token.length() > 0) {
			try {
				value = Integer.valueOf(token);
			}
			catch (NumberFormatException e) {
			}
		}
	}

	@Override
	protected String flush() {
		if (this.token == null) {
			return placeHolder;
		}
		return token;
	}

	private void setValue(int value) {
		this.value = value;
		if (this.value > max) {
			this.value = max;
		}
		else if (this.value < min) {
			this.value = min;
		}
		this.token = formater.format(this.value);
	}

	@Override
	protected void focus(boolean forward) {
		characters.clear();
	}

	@Override
	protected boolean handleKeyDown(int keyDown) {
		switch (keyDown) {
		case KeyCodes.KEY_DOWN:
			if (value > 0) {
				setValue(--value);
			}
			break;
		case KeyCodes.KEY_UP:
			setValue(++value);
			break;
		default:
			break;
		}
		return true;
	}

	@Override
	protected boolean handleKeyPress(char charPressed) {
		if (characters.size() >= maxLenght) {
			return false;
		}
		switch (charPressed) {
		case '0':
			if (characters.isEmpty()) {
				return true;
			}
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			characters.add(charPressed);
			StringBuffer sb = new StringBuffer();
			for (char c : characters) {
				sb.append(c);
			}
			int value = Integer.valueOf(sb.toString());
			if (value <= max && value >= min) {
				setValue(value);
			}
			else {
				characters.remove(characters.size() - 1);
				return false;
			}
			return true;
		default:
			return false;
		}
	}
}