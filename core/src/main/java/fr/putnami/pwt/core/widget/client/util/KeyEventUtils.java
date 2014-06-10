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
package fr.putnami.pwt.core.widget.client.util;

import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.dom.client.NativeEvent;
import com.google.gwt.event.dom.client.KeyCodes;

public final class KeyEventUtils {

	public static final List<Integer> KEYS_SYSTEM = Lists.newArrayList(
			KeyCodes.KEY_ALT, KeyCodes.KEY_BACKSPACE, KeyCodes.KEY_CTRL, KeyCodes.KEY_DELETE,
			KeyCodes.KEY_DOWN, KeyCodes.KEY_END, KeyCodes.KEY_ENTER, KeyCodes.KEY_ESCAPE,
			KeyCodes.KEY_HOME, KeyCodes.KEY_LEFT, KeyCodes.KEY_PAGEDOWN, KeyCodes.KEY_PAGEUP,
			KeyCodes.KEY_RIGHT, KeyCodes.KEY_SHIFT, KeyCodes.KEY_TAB, KeyCodes.KEY_UP
			);

	public static boolean isModifierKeyDown(NativeEvent currentEvent) {
		return currentEvent.getCtrlKey()
				|| currentEvent.getShiftKey()
				|| currentEvent.getMetaKey()
				|| currentEvent.getAltKey();

	}

	private KeyEventUtils() {
	}
}
