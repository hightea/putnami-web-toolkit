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
package fr.putnami.pwt.core.widget.client;

import java.util.Date;

import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasAllFocusHandlers;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.ui.Focusable;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.widget.client.Button.Type;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.helper.CompositeFocusHelper;

public class InputDate extends InputGroup<Date> implements
		DirtyEvent.HasDirtyHandlers,
		HasAllFocusHandlers,
		Focusable,
		HasPlaceholder {

	private final InputDateBox dateBox = new InputDateBox();
	private final Button<Date> calendarButton = new Button<Date>();
	private final ButtonEvent.Handler buttonHandler = new ButtonEvent.Handler() {
		@Override
		public void onButtonAction(ButtonEvent event) {
			toggleDatePicker();
		}

	};

	private InputDatePicker datePicker;

	private CompositeFocusHelper compositeFocusHelper;

	public InputDate() {
		endConstruct();
	}

	protected InputDate(InputDate source) {
		super(source, false);
		endConstruct();
		setFormat(source.getFormat());
		setPlaceholder(source.getPlaceholder());
	}

	protected void endConstruct() {
		calendarButton.setType(Type.ICON);
		calendarButton.setIconType(IconFont.ICON_CALENDAR);
		calendarButton.addButtonHandler(buttonHandler);
		append(dateBox);
		addAddon(calendarButton);
		compositeFocusHelper = CompositeFocusHelper.createFocusHelper(this, dateBox);

	}

	@Override
	public IsWidget cloneWidget() {
		return new InputDate(this);
	}

	public String getFormat() {
		return dateBox.getFormat();
	}

	public void setFormat(String format) {
		dateBox.setFormat(format);
	}

	@Override
	public boolean isDirty() {
		return dateBox.isDirty();
	}

	@Override
	public Date flush() {
		return dateBox.flush();
	}

	@Override
	public void edit(Date value) {
		dateBox.edit(value);
		if (datePicker != null) {
			datePicker.hide();
		}
	}

	@Override
	public HandlerRegistration addDirtyHandler(Handler handler) {
		return dateBox.addDirtyHandler(handler);
	}

	@Override
	public String getPlaceholder() {
		return dateBox.getPlaceholder();
	}

	@Override
	public void setPlaceholder(String placeholder) {
		dateBox.setPlaceholder(placeholder);
	}

	@Override
	public Date getValue() {
		return dateBox.getValue();
	}

	@Override
	public boolean hasErrors() {
		return dateBox.hasErrors();
	}

	@Override
	public Iterable<Error> getErrors() {
		return dateBox.getErrors();
	}

	@Override
	public void addValidator(Validator<Date> validator) {
		dateBox.addValidator(validator);
	}

	private void toggleDatePicker() {
		if (datePicker == null) {
			datePicker = new InputDatePicker();
			datePicker.addValueChangeHandler(new ValueChangeHandler<Date>() {

				@Override
				public void onValueChange(ValueChangeEvent<Date> event) {
					edit(event.getValue());
					datePicker.hide();
					dateBox.setFocus(true);
				}
			});
			compositeFocusHelper.addFocusPartner(datePicker.getElement());
		}
		datePicker.setValue(flush());
		datePicker.togglePopup(this, calendarButton);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addFocusHandler(FocusHandler handler) {
		return compositeFocusHelper.addFocusHandler(handler);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addBlurHandler(BlurHandler handler) {
		return compositeFocusHelper.addBlurHandler(handler);
	}

	@Override
	public int getTabIndex() {
		return dateBox.getTabIndex();
	}

	@Override
	public void setAccessKey(char key) {
		dateBox.setAccessKey(key);
	}

	@Override
	public void setFocus(boolean focused) {
		dateBox.setFocus(focused);
	}

	@Override
	public void setTabIndex(int index) {
		dateBox.setTabIndex(index);
	}

}
