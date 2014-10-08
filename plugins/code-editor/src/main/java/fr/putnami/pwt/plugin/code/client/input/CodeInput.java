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
package fr.putnami.pwt.plugin.code.client.input;

import com.google.gwt.event.dom.client.HasAllFocusHandlers;
import com.google.gwt.event.dom.client.HasKeyDownHandlers;
import com.google.gwt.event.dom.client.HasKeyUpHandlers;
import com.google.gwt.event.logical.shared.HasValueChangeHandlers;
import com.google.gwt.user.client.ui.Focusable;
import com.google.gwt.user.client.ui.HasText;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.plugin.code.client.event.LiveValueChangeEvent.HasLiveValueChangeHandlers;

public interface CodeInput extends HasText, HasPlaceholder, IsWidget, HasLiveValueChangeHandlers,
	HasKeyDownHandlers, HasKeyUpHandlers, HasValueChangeHandlers<String>, HasAllFocusHandlers,
	Focusable {

	int getCursorPosition();

	void setCursorPosition(int cursorPosition);
}
