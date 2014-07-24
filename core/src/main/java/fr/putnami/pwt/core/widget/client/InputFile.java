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

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.DataTransfer;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.InputElement;
//import com.google.gwt.dom.client.DataTransfer;
//import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.Style.Overflow;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.DragLeaveEvent;
import com.google.gwt.event.dom.client.DragLeaveHandler;
import com.google.gwt.event.dom.client.DragOverEvent;
import com.google.gwt.event.dom.client.DragOverHandler;
import com.google.gwt.event.dom.client.DropEvent;
import com.google.gwt.event.dom.client.DropHandler;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.FileUpload;
import com.google.gwt.user.client.ui.FormPanel;
import com.google.gwt.user.client.ui.FormPanel.SubmitCompleteEvent;
import com.google.gwt.user.client.ui.FormPanel.SubmitEvent;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.xhr.client.ReadyStateChangeHandler;
import com.google.gwt.xhr.client.XMLHttpRequest;
import com.google.gwt.xml.client.Document;
import com.google.gwt.xml.client.Text;
import com.google.gwt.xml.client.XMLParser;
import com.google.gwt.xml.client.impl.DOMParseException;

import fr.putnami.pwt.core.service.client.ServiceProxy;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractInput;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.shared.domain.FileDto;
import fr.putnami.pwt.core.widget.shared.domain.UploadStatus;
import fr.putnami.pwt.core.widget.shared.service.UploadService;

public class InputFile extends AbstractInput<FileDto>
implements DropHandler, DragLeaveHandler, DragOverHandler {
	private static final String DOWNLOAD_URL = GWT.getHostPageBaseURL() + "download/";

	private static final CssStyle STYLE_DRAGOVER = new SimpleStyle("file-dragover");

	interface Binder extends UiBinder<Widget, InputFile> {
		Binder BINDER = GWT.create(Binder.class);
	}

	interface UploadServiceRemote extends ServiceProxy<InputFile, UploadService>, UploadService {
	}

	private final UploadServiceRemote uploadService = (UploadServiceRemote) GWT.create(UploadServiceRemote.class);;

	final Timer timer = new Timer() {
		@Override
		public void run() {
			uploadService.getUploadStatus(uploadPostId);
		}
	};

	private static long inputFileBoxCounter = 0;


	@UiField
	FormPanel uploadForm;
	@UiField
	FileUpload fileUpload;
	@UiField
	Label fileName;
	@UiField
	Button deleteBtn;
	@UiField
	Button<?> downloadBtn;
	@UiField
	Button uploadBtn;
	@UiField
	OutputProgressBar<Long> progressBar;

	private boolean uploading;
	private final long uploadPostId = ++inputFileBoxCounter;

	public InputFile() {
		initWidget(Binder.BINDER.createAndBindUi(this));
		uploadService.bindService(this);
	}

	@Override
	protected void initWidget(Widget widget) {
		super.initWidget(widget);
		StyleUtils.removeStyle(this, STYLE_CONTROL);

		uploadForm.getElement().getStyle().setHeight(0, Unit.PX);
		uploadForm.getElement().getStyle().setWidth(0, Unit.PX);
		uploadForm.getElement().getStyle().setOverflow(Overflow.HIDDEN);

		uploadForm.setAction(getActionUrl());

		addDomHandler(this, DragOverEvent.getType());
		addDomHandler(this, DragLeaveEvent.getType());
		addDomHandler(this, DropEvent.getType());

		setValue(null);
	}

	protected InputFile(InputFile source) {
		this();
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputFile(this);
	}

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public void edit(FileDto value) {
		setValue(value);
	}

	@Override
	public FileDto flush() {
		return getValue();
	}

	@Override
	public void setValue(FileDto value) {
		FileDto oldValue = getValue();
		super.setValue(value);
		fileName.setText(null);
		uploading = false;
		progressBar.setVisible(false);
		timer.cancel();
		deleteBtn.setVisible(false);
		downloadBtn.setVisible(false);
		uploadBtn.setVisible(false);
		if (value != null) {
			deleteBtn.setVisible(true);
			downloadBtn.setVisible(true);
			fileName.setText(value.getName());
			((InputElement) fileUpload.getElement().cast()).setValue(null);
		}
		else {
			uploadBtn.setVisible(true);
		}
	}

	@Override
	public HandlerRegistration addValueChangeHandler(ValueChangeHandler<FileDto> handler) {
		return null;
	}

	@UiHandler("deleteBtn")
	void clearValue(ButtonEvent event) {
		setValue(null);
	}

	@UiHandler("uploadBtn")
	public void clickUpload(ButtonEvent event) {
		clickOnInputFile(fileUpload.getElement());
	}

	@UiHandler("downloadBtn")
	public void download(ButtonEvent event) {
		FileDto file = getValue();
		if (file != null) {
			Window.Location.replace(DOWNLOAD_URL + file.getToken());
		}
	}

	@UiHandler("uploadForm")
	public void onSubmit(SubmitEvent event) {

	}

	@UiHandler("uploadForm")
	public void onSubmitComplete(SubmitCompleteEvent event) {
		this.uploading = false;
		FileDto file = parseFile(event.getResults());
		setValue(file);
	}

	@UiHandler("fileUpload")
	public void onFileUploadChange(ChangeEvent event) {
		uploading = true;
		uploadForm.submit();
		initTimerFromFileElement(fileUpload.getElement(), this);
	}

	private void initTimer(String size) {
		progressBar.setMax(Integer.valueOf(size));
		progressBar.edit(0L);
		progressBar.setVisible(true);
		timer.schedule(10);
	}

	private FileDto parseFile(String result) {
		String json = result;
		try {
			Document messageDom = XMLParser.parse(result);
			Text bodyNode = (Text) messageDom.getFirstChild().getFirstChild();
			json = bodyNode.getData();

		}
		catch (DOMParseException e) {
		}

		JSONValue value = JSONParser.parseStrict(json);
		JSONObject jsonObject = value.isObject();
		FileDto file = new FileDto();
		file.setName(jsonObject.get("name").isString().stringValue());
		file.setExtension(jsonObject.get("extension").isString().stringValue());
		file.setMime(jsonObject.get("mime").isString().stringValue());
		file.setToken(jsonObject.get("token").isString().stringValue());
		file.setContentLength((long) jsonObject.get("contentLength").isNumber().doubleValue());
		return file;
	}

	@AsyncHandler
	void onGetUploadStatus(UploadStatus status) {
		if (status != null) {
			progressBar.setMax(Long.valueOf(status.getContentLength()).intValue());
			progressBar.edit(status.getBytesRead());
		}
		if (uploading) {
			timer.schedule(100);
		}

	}

	@Override
	public void onDragLeave(DragLeaveEvent event) {
		StyleUtils.removeStyle(this, STYLE_DRAGOVER);
	}

	@Override
	public void onDragOver(DragOverEvent event) {
		StyleUtils.addStyle(this, STYLE_DRAGOVER);
	}

	@Override
	public void onDrop(DropEvent event) {
		event.preventDefault();
		event.stopPropagation();

		DataTransfer dataTransfer = event.getNativeEvent().getDataTransfer();
		uploaderImpl(dataTransfer, this);

	}

	protected void handleLoad(String base64data, String fileName, String type, String size) {
		String boundary = "xxxxxxx";

		XMLHttpRequest httpRequest = XMLHttpRequest.create();

		httpRequest.setOnReadyStateChange(new ReadyStateChangeHandler() {
			@Override
			public void onReadyStateChange(XMLHttpRequest xhr) {
				if (xhr.getReadyState() == XMLHttpRequest.DONE) {
					xhr.clearOnReadyStateChange();
					timer.cancel();
					//					progressBar.setVisible(false);
					FileDto file = parseFile(xhr.getResponseText());
					setValue(file);
				}
			}
		});

		uploading = true;
		httpRequest.open("POST", getActionUrl());
		GWT.log("uploadFile to " + getActionUrl());
		httpRequest.setRequestHeader("content-type", "multipart/form-data; boundary=" + boundary);
		httpRequest.setRequestHeader("Cache-Control", "max-age=0");

		httpRequest.send("--" + boundary + "\r\n"
				+ "Content-Disposition: form-data; name=\"data\"; filename=\"" + fileName + "\"\r\n"
				+ "Content-Type: " + type + "\r\n\r\n"
				+ base64data
				+ "\r\n--" + boundary + "--");

		initTimer(size);
	}

	private String getActionUrl() {
		return GWT.getHostPageBaseURL() + "uploadfile?uploadPostId=" + uploadPostId;
	}

	private native void uploaderImpl(DataTransfer dataTransfer, InputFile inputFile)
	/*-{
		var files = dataTransfer.files;
		for (var i = 0; i < files.length; i++ ) {
			var file = files[i];
			inputFile.@fr.putnami.pwt.core.widget.client.InputFile::initTimer(Ljava/lang/String;)("" + file.size);
			var b64reader = new FileReader();
			b64reader.onloadend = (function(file) {
			return function(base64) {
				file.base64data = base64.target.result;
				var size = "" + file.size;
				inputFile.@fr.putnami.pwt.core.widget.client.InputFile::handleLoad(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)(file.base64data, file.name, file.type, size);
			}; })(file);
			b64reader.readAsBinaryString(file);
	   }
	}-*/;

	private native void initTimerFromFileElement(Element fileElement, InputFile inputFile)
	/*-{
		var files = fileElement.files;
		fileElement.files = files;
		for (var i = 0; i < files.length; i++ ) {
			var file = files[i];
			inputFile.@fr.putnami.pwt.core.widget.client.InputFile::initTimer(Ljava/lang/String;)("" + file.size);
		}
	}-*/;

	private static native void clickOnInputFile(Element elem)
	/*-{
		elem.click();
	}-*/;

}
