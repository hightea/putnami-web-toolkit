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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Iterator;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.editor.client.factory.InputFactory;
import fr.putnami.pwt.core.editor.client.factory.OutputFactory;
import fr.putnami.pwt.core.widget.client.base.AbstractTableCell;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumn;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumnAspect;

public class TableColumn<T> extends AbstractTableColumn<T> implements HasWidgets.ForIsWidget {

	private OutputFactory outputFactory;
	private InputFactory inputFactory;
	private CloneableWidget widgetFactory;

	private String path;

	private String headerText;

	private TableEditorTH<T> headerCell;

	public TableColumn() {
	}

	protected TableColumn(TableColumn<T> source) {
		super(source);
		this.outputFactory = source.outputFactory;
		this.inputFactory = source.inputFactory;
		this.widgetFactory = source.widgetFactory;
		this.headerText = source.headerText;

		this.path = source.path;
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableColumn<T>(this);
	}

	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof AbstractTableColumnAspect) {
			this.addAspect((AbstractTableColumnAspect<T>) w);
		}
		if (w instanceof OutputFactory) {
			assert this.outputFactory == null : "outputFactory may only be set once";
			this.outputFactory = (OutputFactory) w;
		}
		if (w instanceof InputFactory) {
			assert this.inputFactory == null : "inputFactory may only be set once";
			this.inputFactory = (InputFactory) w;
		}
		if (this.inputFactory == null && this.outputFactory == null && w instanceof CloneableWidget) {
			this.widgetFactory = (CloneableWidget) w;
		}
	}

	@Override
	public TableTH<T> doCreateHeaderCell() {
		if (this.headerCell == null) {
			this.headerCell = new TableEditorTH<T>();
			this.headerCell.setPath(this.path);
			this.headerCell.setColspan(this.getColspan());
			this.headerCell.setText(this.headerText);
			for (AbstractTableColumnAspect aspect : this.getAspects()) {
				if (aspect.getColumnPath() == null) {
					aspect.setColumnPath(this.path);
				}
				this.headerCell.addAspect(aspect);
			}
			this.headerCell.redraw();
		}
		return this.headerCell;
	}

	@Override
	public AbstractTableCell<T> doCreateBodyCell() {
		TableEditorTD<T> bodyCell = new TableEditorTD<T>();
		String subPath = this.path;
		bodyCell.setInputFactory(this.inputFactory);
		bodyCell.setOutputFactory(this.outputFactory);
		bodyCell.setWidgetFactory(this.widgetFactory);
		bodyCell.setColspan(this.getColspan());
		bodyCell.setPath(subPath);
		bodyCell.setReadonly(this.getReadonly());

		return bodyCell;
	}

	@Override
	public void add(Widget child) {
		this.add((IsWidget) child);
	}

	public void setHeaderText(String headerText) {
		this.headerText = headerText;
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException("TableColumn does not support clear()");
	}

	@Override
	public Iterator<Widget> iterator() {
		throw new UnsupportedOperationException("TableColumn does not support iterator()");
	}

	@Override
	public boolean remove(Widget w) {
		throw new UnsupportedOperationException("TableColumn does not support remove(Widget)");
	}

	@Override
	public boolean remove(IsWidget w) {
		throw new UnsupportedOperationException("TableColumn does not support remove(IsWidget)");
	}
}
