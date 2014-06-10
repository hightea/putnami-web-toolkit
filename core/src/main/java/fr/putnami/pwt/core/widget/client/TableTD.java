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

import com.google.gwt.dom.client.TableCellElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.widget.client.base.AbstractTableCell;

public class TableTD<T> extends AbstractTableCell<T> implements
		CloneableWidget,
		HasDrawable {

	private Integer colspan;

	public TableTD() {
		super(TableCellElement.TAG_TD);
	}

	protected TableTD(TableTD<T> source) {
		super(source);
		this.colspan = source.colspan;
		cloneSourceWidgets(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableTD<T>(this);
	}

	@Override
	public void redraw() {
		if (colspan != null) {
			TableCellElement.as(getElement()).setColSpan(colspan);
		}
	}

	public void setColspan(Integer colspan) {
		this.colspan = colspan;
	}

	@Override
	public void add(IsWidget child) {
		append(child);
	}
}
